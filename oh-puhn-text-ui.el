;; -*- lexical-binding: t -*-

;;; code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'reed-elx)
(require 'reed-hooks)
(require 'reed-style)
(require 'reed-render-helper)


(defvar app-name "*oh-puhn-text-ui*")
(defvar pubsub--subscriptions (make-hash-table :test 'eq))

(defcustom oh-puhn-text-ui-model "gemma3:27b"
  "The LLM model to use for completions.
Common values might include \"gpt-4\", \"gpt-3.5-turbo\", \"claude-2\", etc."
  :type 'string
  :group 'oh-puhn-text-ui-settings
  :safe 'stringp)


(defcustom oh-puhn-text-ui-completion-endpoint "http://localhost:11434/v1/chat/completions"
  "The API endpoint URL for LLM completions."
  :type 'string
  :group 'oh-puhn-text-ui-settings
  :safe 'stringp)

(defcustom oh-puhn-text-ui-input-editor-default-mode 'org-mode
  "Default major mode for the input editor."
  :type 'symbol
  :group 'oh-puhn-text-ui-settings)

(defun pubsub-subscribe (event-name handler)
  "Subscribe to EVENT-NAME with HANDLER. Returns unsubscribe function."
  (let ((subscription (cons handler nil)))  ; Create unique cons cell for identification
    (push subscription (gethash event-name pubsub--subscriptions))
    (lambda ()
      (when (gethash event-name pubsub--subscriptions)
        (setf (gethash event-name pubsub--subscriptions)
              (delq subscription
                    (gethash event-name pubsub--subscriptions)))))))

(defun pubsub-emit (event-name &rest args)
  "Emit EVENT-NAME with ARGS to all subscribed handlers."
  (dolist (subscription (gethash event-name pubsub--subscriptions))
    (apply (car subscription) args)))


(defun openai-chat-request (model messages handle-data handle-done)
  "Start OpenAI stream request with PAYLOAD and HANDLE-DATA callback.
Returns abort function to cancel the request."
  ;; (message "[openai-chat-request] %s" messages)
  (let* ((data (json-encode
                `(("model" . ,model)
                  ("messages" . ,messages)
                  ("stream" . t))))
         (args `("-X" "POST"
                 ,oh-puhn-text-ui-completion-endpoint
                 "-H" "Content-Type: application/json"
                 "-d" ,data))
         (process (apply #'start-process `("openai-stream" "*openai*" "curl" ,@args)))
         (buffer "")
         (abort-fn (lambda ()
                     (when (process-live-p process)
                       (kill-process process)
                       (message "Request aborted")))))

    (set-process-filter
     process
     (lambda (proc output)
       (setq buffer (concat buffer output))
       (while (string-match "\n" buffer)
         (let ((line (substring buffer 0 (match-beginning 0)))
               (rest (substring buffer (match-end 0))))
           (setq buffer rest)
           (when (string-prefix-p "data: " line)
             (let ((json-str (substring line 6)))
               (ignore-errors
                 (let* ((json-object (json-parse-string json-str :object-type 'alist))
                        (content (alist-get 'content
                                             (alist-get 'delta
                                                         (elt (alist-get 'choices json-object) 0)))))
                   (when content
                     (funcall handle-data content))))))))))
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (funcall handle-done))))
    abort-fn))


; (defvar tree [])

(defun tree-get-parent (tree id)
  (cadr (aref tree id)))

(defun tree-get-sibling (tree node-id index-diff)
  (let ((siblings (tree-get-children tree (tree-get-parent node-id))))
    (min 0 (max (length siblings) (+ index-diff (cl-position siblings))))))

(defun tree-get-children (tree parent-id)
  (let ((i (1+ (or parent-id -1)))  ; start index
        (len (length tree))
        (result '()))
    (while (< i len)
      (let ((msg (aref tree i)))
        (when (eq (cadr msg) parent-id)
          (setq result (cons i result))))
      (setq i (1+ i)))
    (reverse result)))

(defun tree-get-nth-child (tree parent-id index)
  (car (nthcdr index (tree-get-children tree parent-id))))

(defun tree-add-child (tree parent-id node)
  (let ((node-id (length tree)))
    (vconcat tree (vector `(,node-id ,parent-id . ,node)))))


; (defun tree-get-chat-history-from-parent (from-node-id branches)
;   (let* ((children (tree-get-children from-node-id))
;          (total-children (length children))
;          (selected-child-index (or (car branches) (1- total-children))))
;     (and children
;          (let ((selected-child-id (car (nthcdr selected-child-index children))))
;            (cons (list selected-child-id selected-child-index total-children)
;                  (tree-get-chat-history selected-child-id (cdr branches)))))))

(defun tree-get-latest-leaf (tree node-id)
  (let ((last-child (car (last (tree-get-children tree node-id)))))
    (if last-child
        (tree-get-latest-leaf tree last-child)
      node-id)))

(defun tree-get-chat-id-history-from-leaf (tree leaf-id &optional tail)
  (if leaf-id
      (let ((node (aref tree leaf-id)))
        (tree-get-chat-id-history-from-leaf
         tree
         (cadr node)
         (cons (car node) tail)))
    tail))

(defun tree-append-chunck (tree node-id chunk)
  (let* ((node (aref tree node-id))
         (message-content (car (nthcdr 3 node))))
    (setcar (nthcdr 3 node) (concat message-content chunk))
    tree))


(defun use-subscribe (event f)
  (use-hook-with-cleanup
   (lambda ()
     (pubsub-subscribe event f))
   (lambda (unsub)
     (funcall unsub))))

(defun use-chat-request (conversation-tree-sig leaf-id-sig)
  (let* ((streaming-ref (use-ref (lambda ())))
         (handle-generate (use-callback
                           (lambda (user-node-id)
                             (unless (funcall streaming-ref)
                               (let* ((new-tree (tree-add-child (funcall conversation-tree-sig) user-node-id (list 'assistant nil)))
                                      (assistant-node-id (1- (length new-tree)))
                                      (history-ids (tree-get-chat-id-history-from-leaf new-tree user-node-id))
                                      (message-history (mapcar (lambda (id)
                                                                 (let ((node (cddr (aref new-tree id))))
                                                                   `(("role" . ,(symbol-name (car node)))
                                                                     ("content" . ,(cadr node))))
                                                                 ) history-ids))
                                      (abort (openai-chat-request
                                              oh-puhn-text-ui-model
                                              message-history
                                              (lambda (chunk)
                                                (let* ((tree (funcall conversation-tree-sig))
                                                       (nt (tree-append-chunck tree assistant-node-id chunk)))
                                                  (funcall conversation-tree-sig nt)
                                                  (pubsub-emit 'render)))
                                              (lambda ()
                                                (let* ((tree (funcall conversation-tree-sig))
                                                       (node (aref tree assistant-node-id))
                                                       (message-content (car (nthcdr 3 node))))
                                                  (setcar (nthcdr 3 node)
                                                          (and message-content
                                                               (string-trim message-content)))
                                                  (funcall conversation-tree-sig tree))
                                                (funcall streaming-ref nil)
                                                (pubsub-emit 'render)))))
                                 (funcall conversation-tree-sig new-tree)
                                 (funcall leaf-id-sig assistant-node-id)
                                 (funcall streaming-ref (cons abort assistant-node-id))))))))
    (use-subscribe
     'abort-event
     (lambda ()
       (let ((abort (car (funcall streaming-ref))))
         (when abort
           (funcall abort)
           (funcall streaming-ref nil)))))
    (use-drop
     (lambda ()
       (let ((abort (car (funcall streaming-ref))))
         (when abort
           (funcall abort)
           (funcall streaming-ref nil)))))
    handle-generate))

(defun use-deferred-callback (f)
  (let ((timer-ref (use-ref (lambda ()))))
    (use-drop
     (lambda ()
       (let ((timer (funcall timer-ref)))
         (when timer
           (cancel-timer timer)))))
    (lambda ()
      (funcall timer-ref (run-with-timer 0 nil f)))))

(defun get-editor-window ()
    (let* ((window-width (window-width))
           (window-height (window-height))
           (split-horizontally (> window-width (* window-height 3))))
      (if split-horizontally
          (split-window-right)
        (split-window-below))))

(defun oh-puhn--input-editor (name init-content submit-handler)
  "Open a dedicated buffer with INIT-CONTENT for editing in a split window.
Call SUBMIT-HANDLER with the new content on C-x C-s.
Call SUBMIT-HANDLER with the temporary content when the buffer loses visibility.
Returns a closure to manually close the buffer."
  (let* ((edit-buffer (generate-new-buffer name))
         (original-window (selected-window))
         (had-multiple-windows (> (length (window-list)) 1))
         edit-window)

    ;; Create split window with smart direction based on window size
    (setq edit-window (get-editor-window))

    ;; Switch to the edit window and set up buffer
    (select-window edit-window)
    (switch-to-buffer edit-buffer)

    (with-current-buffer edit-buffer
      (when oh-puhn-text-ui-input-editor-default-mode (funcall oh-puhn-text-ui-input-editor-default-mode))
      (insert init-content)
      (setq header-line-format "Edit buffer. Press C-x C-s to submit, C-g to cancel.")
      (use-local-map (make-sparse-keymap))

      (local-set-key (kbd "C-x C-s")
                     (lambda ()
                       (interactive)
                       (let ((content (buffer-string)))
                         (funcall submit-handler 'submit content)
                         ;; Clean up window and buffer
                         (kill-buffer edit-buffer)
                         (unless had-multiple-windows
                           (delete-window edit-window))
                         (select-window original-window))))

      (local-set-key (kbd "C-g")
                     (lambda ()
                       (interactive)
                       (let ((content (buffer-string)))
                         (funcall submit-handler 'blur content)
                         ;; Clean up window and buffer
                         (kill-buffer edit-buffer)
                         (unless had-multiple-windows
                           (delete-window edit-window))
                         (select-window original-window)))))

    ;; Return close function
    (lambda (&optional action)
      (cond
       ((eq action 'focus)
        (unless (window-live-p edit-window)
          (setq edit-window (get-editor-window)))
        (select-window edit-window)
        (switch-to-buffer edit-buffer))
       (t
        (when (buffer-live-p edit-buffer)
          (let ((content (with-current-buffer edit-buffer
                           (buffer-string))))
            (kill-buffer edit-buffer)
            (when (window-live-p edit-window)
              (unless had-multiple-windows
                (delete-window edit-window))
              (select-window original-window))
            content)))))))

(defun make-spinner-iterator (spinner-frames)
  (let ((i -1)
        (n (length spinner-frames)))
    (lambda ()
      (setq i (mod (1+ i) n))
      (aref spinner-frames i))))

(fc! Spinner ()
     (let ((current-buffer-name (buffer-name))
           (spinner-pos-ref (use-ref (lambda ())))
           (spinner-element-ref (use-ref (lambda ()))))
       (use-after-render
        (use-deferred-callback
         (lambda ()
           (let* ((location (reed-get-absolut-location current-buffer-name (funcall spinner-element-ref)))
                  (x (car location))
                  (y (cdr location))
                  (width (reed-get-width current-buffer-name)))
             (funcall spinner-pos-ref (+ (* (1+ width) y) x 1))))))
       (use-hook-with-cleanup
        (lambda ()
          (let ((current-buffer-name app-name)
                (next-frame (make-spinner-iterator "-\\|/")))
            (run-with-timer
             0 0.2
             (lambda ()

               (let* ((spinner-pos (funcall spinner-pos-ref))
                      (spinner-frame (funcall next-frame)))
                 (when spinner-pos
                   (with-current-buffer current-buffer-name
                     (save-excursion
                       (with-silent-modifications
                         (subst-char-in-region
                          spinner-pos
                          (1+ spinner-pos)
                          (char-after spinner-pos)
                          spinner-frame))))))))))
        (lambda (clear-timer)
          (cancel-timer clear-timer)))
       (elx!
        (p :ref spinner-element-ref " "))))

(fc! UserMessage (conversation-tree-sig leaf-id-sig node index total-swipes ongenerate)
     (let* ((input-editor-ref (use-ref (lambda ())))
            (node-ref (use-ref (lambda ())))
            (close-input-editor (lambda ()
                                  (let ((close (funcall input-editor-ref)))
                                    (when close (funcall close)))))
            (message-content (car (nthcdr 3 node))))
       (funcall node-ref node)
       (use-drop
        (lambda ()
          (let ((close-editor input-editor-ref))
            (when close-editor (funcall close-editor)))))
       (elx!
        (div
         :onclick (lambda (e)
                    (message "[UserMessage] onclick")
                    (funcall
                     input-editor-ref
                     (oh-puhn--input-editor
                      "*oh-puhn-text-ui-input*"
                      message-content
                      (lambda (close-type new-content)
                        (funcall input-editor-ref nil)
                        (if (eq close-type 'submit)
                            (let* ((tree (funcall conversation-tree-sig))
                                   (inner-node (funcall node-ref))
                                   (parent-id (tree-get-parent tree (car inner-node)))
                                   (user-node-id (length tree))
                                   (new-tree (tree-add-child tree parent-id (list 'user new-content))))
                              (funcall conversation-tree-sig new-tree)
                              (funcall leaf-id-sig user-node-id)
                              (funcall ongenerate user-node-id)))))))
         :onblur (lambda (e) (funcall close-input-editor))
         :style (style!*
                 (size
                  (width . 100%)
                  (height . AUTO))
                 (justify_content . '(End)))
         (p
          :style (style!*
                  (border
                   (left . 1pt)
                   (right . 1pt)
                   (top . 1pt)
                   (bottom . 1pt)))
          ({} message-content)))
        ({} (and (> total-swipes 1)
                 (elx!
                  (div
                   :style (style!*
                           (size (width . 100%) (height . AUTO))
                           (justify_content . '(End)))
                   (p ({} (format "<%s/%s>" (1+ index) total-swipes))))))))))

(fc! AssistantMessage (node index total-swipes ongenerate)
     (let* ((regenerate-listener-ref (use-ref (lambda ())))
            (node-ref (use-ref (lambda ())))
            (hovering-ref (use-ref (lambda ())))
            (message-content (car (nthcdr 3 node)))
            (message-content-ref (use-ref (lambda ())))
            (input-editor-ref (use-ref (lambda ()))))
       (funcall node-ref node) ; TODO workaround for id and index binding issue in lambda onhover handler
       (funcall message-content-ref message-content)
       (use-subscribe
        'regenerate-event
        (lambda ()
          (when (funcall hovering-ref)
            (let* ((inner-node (funcall node-ref))
                   (inner-parent-id (cadr inner-node))
                   (inner-role (car (nthcdr 2 inner-node))))
              (when (eq inner-role 'assistant)
                (funcall ongenerate inner-parent-id))))))
       (use-drop
        (lambda ()
          (let ((close-editor input-editor-ref))
            (when close-editor (funcall close-editor)))))
       (elx!
        ({} (and (not message-content) (elx! (Spinner))))
        (p
         :onclick (lambda (e)
                    (message "[click] ass")
                    (funcall
                     input-editor-ref
                     (oh-puhn--input-editor
                      "*oh-puhn-text-ui-assistant-message*"
                      (funcall message-content-ref)
                      (lambda (&rest args)
                        (funcall input-editor-ref nil))))
                    (message "[click] end"))
         :onhover (lambda (e) (funcall hovering-ref t))
         :onleave (lambda (e) (funcall hovering-ref nil))
         :onblur (lambda (e)
                   (let ((close-editor (funcall input-editor-ref)))
                     (when close-editor (funcall close-editor)))
                   (funcall input-editor-ref nil))
         :style (style!*
                 (padding
                  (left . 1pt)
                  (right . 1pt)
                  (top . 1pt)
                  (bottom . 0pt)))
         ({} message-content)
         ({} (if (> total-swipes 1) (format "\n<%s/%s>" (1+ index) total-swipes) nil))))))

(fc! ChatMessage (ref conversation-tree-sig leaf-id-sig id ongenerate onhover onleave)
     (let* ((conversation-tree (funcall conversation-tree-sig))
            (node (aref conversation-tree id))
            (role (car (nthcdr 2 node)))
            (msg (car (nthcdr 3 node)))
            (siblins (tree-get-children conversation-tree (tree-get-parent conversation-tree id)))
            (current-index (cl-position id siblins))
            (swipe-info-ref (use-ref (lambda ())))
            (hovering-sig (use-signal (lambda ()))))
       (funcall swipe-info-ref (list id current-index siblins)) ; TODO workaround for id and index binding issue in lambda onhover handler
       (use-subscribe
        'swipe-event
        (lambda (direction)
          (when (funcall hovering-sig)
            (let* ((swipe-info (funcall swipe-info-ref))
                   (inner-id (car swipe-info))
                   (inner-current-index (cadr swipe-info))
                   (inner-siblins (caddr swipe-info))
                   (inner-tree (funcall conversation-tree-sig)))
              (let* ((siblin (car
                              (nthcdr
                               (+ inner-current-index (if (eq direction 'left) -1 1))
                               inner-siblins))))
                (when (and siblin (not (equal siblin inner-id)))
                  (funcall leaf-id-sig (tree-get-latest-leaf inner-tree siblin))))))))
       (elx!
        (div
         :ref ref
         :face (if (funcall hovering-sig) 'highlight nil)
         :style (style!* (display . 'Block))
         :onhover (lambda (e)
                    (funcall hovering-sig t)
                    (funcall onhover))
         :onleave (lambda (e)
                    (funcall hovering-sig nil)
                    (funcall onleave))
         ({} (if (eq role 'user)
                 (elx! (UserMessage
                        :conversation-tree-sig conversation-tree-sig
                        :leaf-id-sig leaf-id-sig
                        :node node
                        :index current-index
                        :total-swipes (length siblins)
                        :ongenerate ongenerate))
               (elx! (AssistantMessage
                      :node node
                      :index current-index
                      :total-swipes (length siblins)
                      :ongenerate ongenerate))))))))


(fc! InputArea (onsubmit)
     (let* ((input-editor-ref (use-ref (lambda ())))
            (value-sig (use-signal (lambda () "")))
            (value (funcall value-sig))
            (close-input-editor (lambda ()
                                  (let ((close (funcall input-editor-ref)))
                                    (when close (funcall close)))))
            (element-ref (use-ref (lambda ()))))
       (use-drop close-input-editor)
       (use-subscribe
        'edit-event
        (lambda ()
          (when (funcall element-ref)
            (reed-emit-event app-name 'click (funcall element-ref) '() nil))))
       (elx!
        (p
         :ref element-ref
         :onclick (lambda (e)
                    (let ((editor-handle (funcall input-editor-ref)))
                      (if editor-handle
                          (funcall editor-handle 'focus)
                        (funcall input-editor-ref
                                 (oh-puhn--input-editor
                                  "*oh-puhn-text-ui-input*"
                                  (funcall value-sig)
                                  (lambda (close-type new-content)
                                    (funcall input-editor-ref nil)
                                    (if (eq close-type 'submit)
                                        (progn
                                          (funcall value-sig "")
                                          (funcall onsubmit new-content))
                                      (funcall value-sig new-content))))))))
         :onblur (lambda (e) (funcall close-input-editor))
         :style (style!* (display . 'Block)
                         (border
                          (left . 0pt)
                          (right . 0pt)
                          (top . 1pt)
                          (bottom . 0pt)))
         ({} (if (equal value "")
                 (elx! (span :face 'shadow "<Press 'Enter' here to message assistant...>"))
               value))))))

(defconst oh-puhn-logo
"
       _                   _
      | |                 | |
  ___ | |__    _ __  _   _| |__  _ __
 / _ \\| '_ \\  | '_ \\| | | | '_ \\| '_ \\
| (_) | | | | | |_) | |_| | | | | | | |
 \\___/|_| |_| | .__/ \\__,_|_| |_|_| |_|
              | |
              |_|")

(fc! Wellcome ()
     (elx!
      (div
       :style (style!*
               (justify_content . '(Center))
               (align_items . '(Center))
               (size
                (width . 100%)
                (height . AUTO))
               (min_size
                (width . 100%)
                (height . 25pt))
               (flex_direction . 'Column))
       (p ({} oh-puhn-logo))
       (p "How can I help you today?\n" (span :face 'shadow "`C-x e` to start writing your first message.")))))

(fc! App ()
     (let* ((conversation-tree-sig (use-signal (lambda () [])))
            (conversation-tree (funcall conversation-tree-sig))
            (leaf-id-sig (use-signal (lambda ()
                                       (tree-get-latest-leaf
                                        (funcall conversation-tree-sig)
                                        nil))))
            (message-id-list-memo
             (use-memo (lambda ()
                         (let ((res (tree-get-chat-id-history-from-leaf
                                (funcall conversation-tree-sig)
                                (funcall leaf-id-sig))))
                           res))))
            (handle-generate (use-chat-request conversation-tree-sig leaf-id-sig))
            (message-elements-ref (use-ref (lambda () (make-hash-table :test 'eq))))
            (handle-submit
             (use-callback
              (lambda (user-msg-content)
                (let* ((conversation-tree (funcall conversation-tree-sig))
                       (user-node-id (length conversation-tree)))
                  (funcall conversation-tree-sig
                           (tree-add-child conversation-tree (funcall leaf-id-sig) (list 'user user-msg-content)))
                  (funcall handle-generate user-node-id)
                  (pubsub-emit 'render)))))
            (focusing-ref (use-ref (lambda ()))))
       (use-subscribe
        'hover-message-event
        (lambda (direction)
          (let ((ids (funcall message-id-list-memo))
                (hovering-id (funcall focusing-ref))
                (message-element-hashtable (funcall message-elements-ref)))
            (when ids  ; do nothing if list is empty
              (let ((current-index (if hovering-id
                                       (cl-position hovering-id ids :test #'equal)
                                     -1))  ; treat nil as before first element
                    (next-index nil)
                    (element-location nil))
                (setq next-index
                      (if (eq direction 'previous)
                          (max 0 (1- (or current-index 0)))
                        (min (1+ (or current-index -1)) (1- (length ids)))))
                (setq element-location
                      (reed-get-absolut-location
                       (buffer-name)
                       (gethash (nth next-index ids) message-element-hashtable)))
                (goto-char (+ 1 (* (cdr element-location) (1+ (window-width))) (car element-location))))))))
       (elx!
        (div
         :style (style!*
                 (size
                  (width . 100%)
                  (height . AUTO))
                 (flex_direction . 'Column))
         ({} (if (= 0 (length conversation-tree))
                 (elx! (Wellcome))
               (mapcar
                (lambda (id)
                  (elx!
                   (ChatMessage
                    :ref (lambda (&rest ref)
                           (let ((message-element-hashtable (funcall message-elements-ref)))
                             (if ref
                                 (puthash id (car ref) message-element-hashtable)
                               (gethash id message-element-hashtable))))
                    :conversation-tree-sig conversation-tree-sig
                    :leaf-id-sig leaf-id-sig
                    :id id
                    :ongenerate handle-generate
                    :onhover (lambda ()
                               (funcall focusing-ref id))
                    :onleave (lambda ()
                               (when
                                   (eq (funcall focusing-ref) id)
                                 (funcall focusing-ref nil))))))
                (funcall message-id-list-memo))))
         (InputArea :onsubmit handle-submit)))))


(defvar last-post-command-position 1)

(defvar last-buffer-width (window-width))

(defun oh-puhn-text-ui-handle-render ()
  (with-current-buffer (get-buffer-create app-name)
    (let* ((old-pos (window-start))
           (res (reed-render-immediate app-name))
           (duration nil)
           (content (car res))
           (faces (cdr res)))
      (when res
        (with-silent-modifications
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; (set-text-properties (point-min) (point-max) nil)
            )
          (insert content)
          ;;(message "[edit-instruction] %s" edit-instruction)
          ;; (mapc (lambda (inst)
          ;;         (delete-region (1+ (car inst)) (1+ (cadr inst)))
          ;;         (goto-char (1+ (car inst)))
          ;;         (insert (caddr inst)))
          ;;       edit-instruction)
          (mapc (lambda (face) (apply #'add-face-text-property face)) faces)
          (goto-char last-post-command-position)
          (set-window-start (selected-window) old-pos t))
        (reed-handle-cursor-event app-name 'move last-post-command-position '())))))

(defun scale-windows-width (width)
  (1- (/ (* width 218) 224))
  width)

(setq redisplay-dont-pause t)
(setq redisplay-skip-fontification-on-input t)
(defun do-stuff-if-moved-post-command ()
  (when (equal app-name (buffer-name (current-buffer)))
    (let ((should-render nil))
      (when (not (equal (point) last-post-command-position))
        (setq last-post-command-position (point))
        (setq should-render t)
        (reed-handle-cursor-event app-name 'move last-post-command-position '()))
      (when (not (= last-buffer-width (window-width)))
        (setq last-buffer-width (window-width))
        (setq should-render t)
        (reed-set-width app-name (scale-windows-width last-buffer-width)))
      (when should-render
        (oh-puhn-text-ui-handle-render)))))



(defun oh-puhn-text-ui-handle-click ()
  (interactive)
  (message "[oh-puhn] Click at position %s" last-post-command-position)
  (reed-handle-cursor-event app-name 'click last-post-command-position '())
  (oh-puhn-text-ui-handle-render))

(defun oh-puhn-text-ui-swipe-left ()
  (interactive)
  (message "[oh-puhn] Swiping left")
  (pubsub-emit 'swipe-event 'left)
  (oh-puhn-text-ui-handle-render))

(defun oh-puhn-text-ui-swipe-right ()
  (interactive)
  (message "[oh-puhn] Swiping right")
  (pubsub-emit 'swipe-event 'right)
  (oh-puhn-text-ui-handle-render))

(defun oh-puhn-text-ui-regenerate ()
  (interactive)
  (message "[oh-puhn] Regenerating content...")
  (pubsub-emit 'regenerate-event)
  (oh-puhn-text-ui-handle-render))

(defun oh-puhn-text-ui-abort ()
  (interactive)
  (message "[oh-puhn] Aborting streaming")
  (pubsub-emit 'abort-event)
  (oh-puhn-text-ui-handle-render))

(defun oh-puhn-text-open-editor ()
  (interactive)
  (message "[oh-puhn] Opening editor")
  (pubsub-emit 'edit-event)
  (with-current-buffer app-name
    (do-stuff-if-moved-post-command)))

(defun oh-puhn-hover-previous-message ()
  (interactive)
  (message "[oh-puhn] Previous message")
  (pubsub-emit 'hover-message-event 'previous))

(defun oh-puhn-hover-next-message ()
  (interactive)
  (message "[oh-puhn] Next message")
  (pubsub-emit 'hover-message-event 'next))


(defun oh-puhn-text-ui-key-binding ()
  (with-current-buffer app-name
    (local-set-key (kbd "RET") #'oh-puhn-text-ui-handle-click)
    (local-set-key (kbd "[") #'oh-puhn-text-ui-swipe-left)
    (local-set-key (kbd "]") #'oh-puhn-text-ui-swipe-right)
    (local-set-key (kbd "C-r") #'oh-puhn-text-ui-regenerate)
    (local-set-key (kbd "C-c C-c") #'oh-puhn-text-ui-abort)
    (local-set-key (kbd "C-x e") #'oh-puhn-text-open-editor)
    (local-set-key (kbd "M-p") #'oh-puhn-hover-previous-message)
    (local-set-key (kbd "M-n") #'oh-puhn-hover-next-message)
    ;;(local-set-key [down-mouse-1] #'oh-puhn-text-ui-handle-click)
    ))


(pubsub-subscribe 'render (lambda () (oh-puhn-text-ui-handle-render)))
(reed-register-app app-name #'App)
(reed-init-tracing)

(defun oh-puhn-text-ui ()
  (interactive)
  (with-current-buffer (get-buffer-create app-name)
    ;(setq buffer-read-only t)
    (buffer-disable-undo)
    (add-to-list 'post-command-hook #'do-stuff-if-moved-post-command)
    (reed-set-width app-name (scale-windows-width last-buffer-width))
    (switch-to-buffer app-name)
    (oh-puhn-text-ui-key-binding)
    (setq-local truncate-lines t)
    (oh-puhn-text-ui-handle-render)))

;; (oh-puhn-text-ui)
(provide 'oh-puhn-text-ui)
;;; oh-puhn-text-ui.el ends here

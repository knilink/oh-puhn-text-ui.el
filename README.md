# oh-puhn-text-ui.el
Experimental project to the experimental [reed.el](https://github.com/knilink/reed.el).

## Setup
Clone and Build reed
``` sh
git clone https://github.com/knilink/reed.el
cd reed.el
git checkout 17597a0
cargo build --release
```
Clone this repo
```sh
git clone https://github.com/knilink/oh-puhn-text-ui.el
```
Load the dynamic module and elisp script to emacs.d as well as customize completion endpoint and model.
```elisp
(load "/path/to/reed.el/target/release/libreed.so")
(add-to-list 'load-path "/path/to/reed.el/lisp")
(load "/path/to/oh-puhn-text-ui.el/oh-puhn-text-ui.el")

(setq oh-puhn-text-ui-model "gemma3:27b")
(setq oh-puhn-text-ui-completion-endpoint "http://localhost:11434/v1/chat/completions")
```

`M-x oh-puhn-text-ui` to run the app.


## Bindings
### oh-puhn-text-ui
- `RET`: `oh-puhn-text-ui-handle-click`, open input buffer for user message content at cursor position
- `[`: `oh-puhn-text-ui-swipe-left`, swipe to previous message at cursor position
- `]`: `oh-puhn-text-ui-swipe-right`, swipe to next message at cursor position
- `C-r`: `oh-puhn-text-ui-regenerate`, regenerate assistant message at cursor position
- `C-c C-c`: `oh-puhn-text-ui-abort`, abort streaming
- `C-x e`: `oh-puhn-text-open-editor` open new message input editor
- `M-p`: `oh-puhn-hover-previous-message` move cursor to previous chat message
- `M-n`: `oh-puhn-hover-next-message` move cursor to previous chat message

### oh-puhn-text-ui-input
- `C-x s`: save and submit user message content
- `C-g`: abort editing

## Demo
[demo.webm](https://github.com/user-attachments/assets/6c05176b-7a75-49d9-bb77-b5a847cc544d)

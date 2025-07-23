(in-package :lem-user)

;; Font

#+lem-sdl2
(progn
  (let ((font "/Users/vinicius.simoes/Library/Fonts/IosevkaNerdFont-Regular.ttf")
        (bold-font "/Users/vinicius.simoes/Library/Fonts/IosevkaNerdFont-Bold.ttf"))
    (when (and (probe-file font) (probe-file bold-font))
      (lem-sdl2/display:change-font (lem-sdl2/display:current-display)
                                    (lem-sdl2/font:make-font-config
                                     :latin-normal-file font
                                     :latin-bold-file bold-font
                                     :size 16)))))
;; Completion system

(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)
(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode::completion-end)))

;; Keybindings

(define-keys *global-keymap*
  ("C-x F" 'lem-core/commands/file:find-file-recursively)
  ("C-h b" 'describe-bindings)
  ("C-h k" 'describe-key)
  ("C-h a" 'lem-lisp-mode:lisp-apropos)
  ("C-h c" 'apropos-command)
  ("C-h p" 'lem-lisp-mode:lisp-apropos-package)
  ("C-h f" 'lem-lisp-mode:lisp-describe-symbol))

;; Custom commands

(define-command open-init-file ()
    (lem:find-file (merge-pathnames "init.lisp" (lem-home))))

;; Modes

(lem-paredit-mode:paredit-mode t)
(lem/line-numbers::line-numbers-mode t)

;; Trailing spaces before save

(add-hook lem/buffer/file:before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; elixir mode lsp

(setf (lem-lsp-mode/spec:spec-command
       (lem-lsp-mode/spec:get-language-spec 'lem-elixir-mode:elixir-mode))
      '("sh" "~/.emacs.d/cache/lsp/elixir-ts-mode/lexical/bin/start_lexical.sh"))

;; Meta as CMD on MacOS (hack way)

(in-package :lem-sdl2/keyboard)

(defparameter *modifier-code-table*
  `((:shift ,sdl2-ffi:+kmod-lshift+ ,sdl2-ffi:+kmod-rshift+)
    (:ctrl ,sdl2-ffi:+kmod-lctrl+ ,sdl2-ffi:+kmod-rctrl+)
    (:meta ,sdl2-ffi:+kmod-lalt+)
    (:super ,sdl2-ffi:+kmod-lgui+ ,sdl2-ffi:+kmod-rgui+)))

(defun get-modifier (keysym)
  (let* ((mod (sdl2:mod-value keysym))
         (shift (mod-p mod :shift))
         (ctrl (mod-p mod :ctrl))
         (meta (mod-p mod :meta))
         (super (mod-p mod :super)))
    (make-modifier :shift shift :ctrl ctrl :meta meta :super super)))

(require 'tempo)

(tempo-define-template "elixir-mode-pipe-inspect"
                       '("|> IO.inspect(label: \"" p "\")")
                       "pi"
                       "Pipe with IO.inspect"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-pipe"
                       '("|> " p)
                       "p"
                       "Inserts a pipe"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-type"
                       '("@type t :: %__MODULE__{" n> p n "}" >)
                       "type"
                       "Inserts a type template"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-defdelegate"
                       '("defdelegate " p ", to: " p " as: " p )
                       "defdelegate"
                       "Inserts a defdelegate template"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-defmodule-filename"
                       '("defmodule "
                         (string-replace
                          "_" ""
                          (string-replace
                           "/" "."
                           (substring
                            (capitalize
                             (cadr
                              (split-string
                               (file-name-directory buffer-file-name) "lib")))
                            1)))
                         (mapconcat 'capitalize (split-string (file-name-base) "_") "")
                         " do"
                         n n
                         "end" >)
                       "defmodule"
                       "Inserts a defmodule with the name gereated from file name."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-defmodule"
                       '("defmodule " p " do" n p n "end" >)
                       "defm"
                       "Inserts a defmodule template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-moduledoc"
                       '("@moduledoc \"\"\"" > n p n "\"\"\"" >)
                       "moddoc"
                       "Inserts a module doc template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-doc"
                       '("@doc \"\"\"" > n p n "\"\"\"" >)
                       "doc"
                       "Inserts a doc template."
                       'elixir-tempo-tags)

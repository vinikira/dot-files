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
                       '("defdelegate " p ", to: " p ", as: " p )
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

(tempo-define-template "elixir-mode-describe"
                       '("describe \"" p "\" do" > n > p n "end" >)
                       "describe"
                       "Inserts a describe template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-test"
                       '("test \"" p "\" do" > n > p n "end" >)
                       "test"
                       "Inserts a test template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-test-context"
                       '("test \"" p "\", ctx" p " do" > n > p n "end" >)
                       "testc"
                       "Inserts a test with context template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-for"
                       '("for " p " <- " p " do" > n > p n "end" >)
                       "for"
                       "Inserts a for template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-def"
                       '("def " p "(" p ") do" > n > p n "end" >)
                       "def"
                       "Inserts a function template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-defp"
                       '("defp " p "(" p ") do" > n > p n "end" >)
                       "defp"
                       "Inserts a private function template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-fn"
                       '("fn " p " -> " > p " end")
                       "fn"
                       "Inserts an anonymous function template."
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-pipe-debug"
                       '("|> dbg()")
                       "pd"
                       "Pipe with dbg()"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-pipe-io-puts"
                       '("IO.puts(\"" p "\")")
                       "iop"
                       "IO puts"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-pipe-io-inspect"
                       '("IO.inspect(" p ", label: \"" p "\")")
                       "ioi"
                       "IO inspect"
                       'elixir-tempo-tags)

(tempo-define-template "elixir-mode-pry"
                       '("require IEx; IEx.pry();")
                       "pry"
                       "IEx pry"
                       'elixir-tempo-tags)

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

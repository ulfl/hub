let map
    : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
    =   λ(a : Type)
      → λ(b : Type)
      → λ(f : a → b)
      → λ(xs : List a)
      → List/build
        b
        (   λ(list : Type)
          → λ(cons : b → list → list)
          → List/fold a xs list (λ(x : a) → cons (f x))
        )

let Command : Type = { tags : List Text, shellCommand : Text }

let cmd = λ(t : List Text) → λ(c : Text) → { tags = t, shellCommand = c }

let tags =
        λ(t : List Text)
      → λ(cmds : List Command)
      → map
        Command
        Command
        (λ(c : Command) → { tags = t # c.tags, shellCommand = c.shellCommand })
        cmds

let cmds =
        λ(tags : List Text)
      → λ(vars : List Text)
      → λ(fmt : Text → Text)
      → map Text Command (λ(var : Text) → cmd tags (fmt var)) vars

let web =
        λ(t : List Text)
      → λ(c : Text)
      → { tags = t, shellCommand = (env:OPENCMD as Text ? "open") ++ " " ++ c }

in  [ Command, cmd, tags, cmds, web ]

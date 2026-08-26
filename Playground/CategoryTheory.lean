def identity {α : Type} (x : α) : α :=
  x

def compose {α β γ : Type} (f : β → γ) (g : α → β) : α → γ :=
  fun x => f (g x)

theorem compose_identity_left
    {α β : Type} (f : α → β) :
    compose identity f = f := by
  funext x
  rfl

theorem compose_identity_right
    {α β : Type} (f : α → β) :
    compose f identity = f := by
  funext x
  rfl

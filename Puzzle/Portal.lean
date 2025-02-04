/-- A portal! -/
inductive Portal
| primary
| alternate
deriving BEq

def Portal.complement : Portal → Portal
| primary => alternate
| alternate => primary

instance : ToString Portal where
  toString
  | .primary => "blue"
  | .alternate => "orange"

instance : Repr Portal where
  reprPrec portal _prec := toString portal

/-- A portal! -/
inductive Portal
| primary
| alternate
deriving BEq, Repr

def Portal.compliment : Portal → Portal
| primary => alternate
| alternate => primary

namespace LeanVerif.Proverif.Pvqueue

/-- Internal queue state using two lists (front/back). -/
structure queue_state (α : Type) where
  front : List α
  back : List α

def queue_state.normalize (q : queue_state α) : queue_state α :=
  match q.front with
  | [] => { front := q.back.reverse, back := [] }
  | _ => q

/-- Materialize the queue into a list in FIFO order. -/
def queue_state.toList (q : queue_state α) : List α :=
  q.front ++ q.back.reverse

/-- Mutable queue wrapper. -/
structure q (α : Type) where
  ref : IO.Ref (queue_state α)

/-- Create a new empty queue. -/
def new_queue : IO (q α) := do
  let r ← IO.mkRef { front := ([] : List α), back := [] }
  pure { ref := r }

/-- Add an element to the queue. -/
def add (queue : q α) (elem : α) : IO Unit := do
  queue.ref.modify (fun q => { q with back := elem :: q.back })

/-- Get and remove the head of the queue. -/
def get (queue : q α) : IO (Option α) := do
  let q0 ← queue.ref.get
  let q1 := q0.normalize
  match q1.front with
  | [] =>
      queue.ref.set q1
      pure none
  | x :: xs =>
      queue.ref.set { q1 with front := xs }
      pure (some x)

/-- Queue length. -/
def length (queue : q α) : IO Nat := do
  let q0 ← queue.ref.get
  pure (q0.front.length + q0.back.length)

/-- Check whether some element satisfies {lit}`f`. -/
def «exists» (queue : q α) (f : α → Bool) : IO Bool := do
  let q0 ← queue.ref.get
  pure (q0.toList.any f)

/-- Filter queue in place using {lit}`f`. -/
def filter (queue : q α) (f : α → Bool) : IO Unit := do
  queue.ref.modify (fun q =>
    { front := (q.toList.filter f)
      back := [] })

/-- Iterate over elements in FIFO order with {lit}`f`. -/
def iter (queue : q α) (f : α → IO Unit) : IO Unit := do
  let q0 ← queue.ref.get
  for x in q0.toList do
    f x

end LeanVerif.Proverif.Pvqueue

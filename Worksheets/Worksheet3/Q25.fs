type ItemState = InSack | OutofSack | Unknown
type Partial = {Items: (Item * ItemState) list}
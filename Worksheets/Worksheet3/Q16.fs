type TRole = | EEE | EIE | Staff
type TUGYear = | One | Two | Three | Four
type Person = {eeid: int; name: string; role: TRole; year: TUGYear option; cid: int}
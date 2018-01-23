#time "on"
#load "Q32.fs"

open Q32

let squared x = x*x

//mapTailRecRev squared [1..50000]

//mapTailRecAppend squared [1..50000]

List.map squared [1..50000]

#time "off"
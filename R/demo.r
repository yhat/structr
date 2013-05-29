
mylist <- list.py(1, 2, 3, 4, 5)
mylist
mylist$append(6)
mylist$append(-1)
mylist
sum(mylist)
sign(mylist)

mylist$map(function(x) {
  print(x * 100)
})

mylist
mylist$find(function(x) { x > 2 })
mylist <- list.py("greg", "austin", "gregory")
mylist
mylist$append("george")
mylist
mylist$prepend("mary")
mylist
mylist$find("^g")
mylist
mylist$index("greg")
mylist
mylist$insert("tom", 1)
mylist
mylist$sort()
mylist

mylist <- list.py("greg", 1, TRUE, "greg")
mylist
mylist$sort()
mylist
mylist[1]
mylist

each(mylist, function(x) {
  print(x)
})
mylist

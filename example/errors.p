mut x = 0
for 1 to 0 {
  for 2 to 3 {
    for 3 to 4 {
      x = x / 0
    }
  }
}
for 0 to 1 {
  y = x * 0 // Line number is only correct in properly formatted sources
}

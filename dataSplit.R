# Spliting functions
block_wise_split = function(images, k_x = 2, k_y = 5, 
                            percentage=c(0.8, 0.2), 
                            valpercentage = -1, 
                            delete_unlabel = TRUE) {
  # Some initialization
  counter = 0
  blocks = list()
  
  # looping through images
  for (i in 1:length(images)) {
    img = images[[i]]
    if (delete_unlabel) {
      img=img[which(img$label != "Unlabeled"), ]
    }
    
    # calculating the split points
    img_x_max = max(img$x)
    img_x_min = min(img$x)
    
    img_y_max = max(img$y)
    img_y_min = min(img$y)
    split_x = c(0, 
                img_x_min + as.integer((img_x_max-img_x_min)/k_x) * c(1:(k_x-1)), 
                img_x_max)
    split_y = c(0, 
                img_y_min + as.integer((img_y_max-img_y_min)/k_y) * c(1:(k_y-1)), 
                img_y_max)
    
    # Splitting: loop start at 2 because the first split point is 0
    for (j in 2:(k_x+1)) {
      for (k in 2:(k_y+1)) {
        counter = counter + 1
        xs_prev = split_x[j-1]; xs_curr = split_x[j]
        ys_prev = split_y[k-1]; ys_curr = split_y[k]
        block = img[which((img$x > xs_prev) & (img$x <= xs_curr) &
                            (img$y > ys_prev) & (img$y <= ys_curr)), ]
        blocks[[counter]] = block
      }
    }
  }
  
  # sample the blocks by length and percentages to create train/validation/test splits
  test_length = as.integer(percentage[2] * length(blocks))
  test_index = sample(1:length(blocks), test_length)
  test_blocks = blocks[test_index]
  train_blocks = blocks[-test_index]
  val_blocks = NA
  if (valpercentage != -1) {
    val_length= as.integer(valpercentage * length(train_blocks))
    val_index = sample(1:length(train_blocks), val_length)
    val_blocks = train_blocks[val_index]
    train_blocks = train_blocks[-val_index]
  }
  return (list("train_blocks"=train_blocks,
               "val_blocks" = val_blocks, 
               "test_blocks" = test_blocks))
}



vert_wise_split = function(images, k=10, 
                           percentage = c(0.8, 0.2), 
                           valpercentage = -1,
                           delete_unlabel = TRUE) {
  blocks = list()
  counter = 0
  # very similar procedure to block_wise_split
  for (i in 1:length(images)) {
    img = images[[i]]
    if (delete_unlabel) {
      img=img[which(img$label != "Unlabeled"), ]
    }
    split_max = max(img$x)
    split_min = min(img$x)
    split_x = c(0, 
                split_min + as.integer((split_max-split_min)/k) * c(1:(k-1)), 
                split_max)
    for (j in 2:(k+1)) {
      counter = counter + 1
      xs_prev = split_x[j-1]; xs_curr = split_x[j]
      blocks[[counter]] = img[which((img$x > xs_prev) & (img$x <= xs_curr)), ]
    }
  }
  test_length = as.integer(percentage[2] * length(blocks))
  test_index = sample(1:length(blocks), test_length)
  test_blocks = blocks[test_index]
  train_blocks = blocks[-test_index]
  val_blocks = NA
  if (valpercentage != -1) {
    val_length= as.integer(valpercentage * length(train_blocks))
    val_index = sample(1:length(train_blocks), val_length)
    val_blocks = train_blocks[val_index]
    train_blocks = train_blocks[-val_index]
  }
  return (list("train_blocks"=train_blocks,
               "val_blocks" = val_blocks, 
               "test_blocks" = test_blocks))
}
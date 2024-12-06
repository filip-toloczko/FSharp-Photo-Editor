//
// Project 02 : Image processing in F#
// Description: In this project we will perform various operations on a ppm image using F#. These operation
//              include grayscaling an image, thresholding an image, flipping an image, detecting
//              edges in an image, and rotating an image to the right by 90 degrees.
// Filip Toloczko
// NetID - ftolo2
// UIN - 662265995
// 10/28/2023
//

namespace ImageLibrary

module Operations =

  //Helper function for Grayscale that recursively replaces each triple in a list of lists 
  //with a pixel with a gray value
  let rec _Grayscale rowOfPixels = 
    match rowOfPixels with
    | [] -> [] //Base case
    | (r,g,b)::tail -> 
        //The value of the pixels is set to red * 0.299 + green * 0.59 + blue * 0.114
        //In the calculation, convert red, green and blue to floats, and then convert the final value to an int
        let grayValue:int = int (float r * 0.299 + float g * 0.587 + float b * 0.114)
        (grayValue, grayValue, grayValue)::_Grayscale tail//Prepend the triple with the gray values and run _Grayscale on the tail

  //Function that returns a grayscale image of a given image
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    match image with
    | [] -> [] //Base case
    | head::tail ->
        let grayRow = _Grayscale head //Pass head into _Grayscale to get a row of grayscale pixels
        grayRow::Grayscale width height depth (tail:(int*int*int) list list) //Prepend the row and run Grayscale on the tail
    
  //Helper function for Threshold. Modifies the r,g, and b values of a triple when compared to a threshold value
  let rec _Threshold rowOfPixels threshold = 
    match rowOfPixels with
      | [] -> [] //Base case
      | (r,g,b)::tail -> 
          let newR = 
            if r > threshold then 255
            else 0
          let newG = 
            if g > threshold then 255
            else 0
          let newB = 
            if b > threshold then 255
            else 0
            
          (newR, newG, newB)::_Threshold tail threshold //Prepend the new rgb values and run _Threshold with tail

  //Function that returns a threshold image from an image
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    match image with
    | [] -> [] //Base case
    | head::tail ->
        let thresholdRow = _Threshold head threshold //Pass head into threshold to get a row of updated pixels
        //Prepend the new row and run threshold on tail
        thresholdRow::Threshold width height depth (tail:(int*int*int) list list) threshold

  //Function to flip an image accross its horizontal axis
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with
    | [] -> [] //Base case
    | head::tail -> 
        let reversedList = List.rev head //Set reversedList to the reversed version of the head row
        //Prepend the new row and run FlipHorizontal on tail
        reversedList::FlipHorizontal width height depth (tail:(int*int*int) list list)

  //Helper function for EdgeDetect. Sets a pixel to be either black or white depending on whether or not it is an edge
  let rec _EdgeDetect (image:(int*int*int) list list) threshold row column width height (edgedRow:(int*int*int) list) (edgedImage:(int*int*int) list list) = 
    //If the current row is the last row, return the updated image
    if(row = height-1) then
      edgedImage

    //If the current column is the last column move on to the next rows first column
    //Prepend the row of new pixels to image 
    else if(column = width-1) then
      let newRow = row+1
      let resetColumn = 0
      let tempEdgedRow = edgedRow
      _EdgeDetect image threshold newRow resetColumn width height [] (tempEdgedRow::edgedImage)

    else 
      if(column < width-1) then
        //Define the 3 locations, the current, the one to the right and the one below
        let current = image.[row].[column]
        let right = image.[row].[column+1]
        let down = image.[row+1].[column]
        //Convert these locations into triple values
        let (xCurr, yCurr, zCurr) = current
        let (xRight, yRight, zRight) = right
        let (xDown, yDown, zDown) = down

        //Find Distances from the current pixel to the pixel to the right of it
        let distanceRight = 
          sqrt((float(xCurr-xRight)*float(xCurr-xRight))+(float(yCurr-yRight)*float(yCurr-yRight))+(float(zCurr-zRight)*float(zCurr-zRight)))
        //Find Distances from the current pixel to the pixel under it
        let distanceDown = 
          sqrt((float(xCurr-xDown)*float(xCurr-xDown))+(float(yCurr-yDown)*float(yCurr-yDown))+(float(zCurr-zDown)*float(zCurr-zDown)))

        //Check if the given pixel is an edge
        if(distanceRight>float(threshold) || distanceDown>float(threshold)) then 
          //If so make it black
          let newLowElement = (0,0,0)
          _EdgeDetect image threshold row (column+1) width height (newLowElement::edgedRow) edgedImage
          
        else
          //Make it white
          let newHighElement = (255,255,255)
          _EdgeDetect image threshold row (column+1) width height (newHighElement::edgedRow) edgedImage
          
      else 
        edgedImage
        
  //Function that finds all of the edges in an image. If an edge is found, the pixel is turned black
  //Otherwise the pixel is turned white
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 

    //Create two empty lists to pass into helper function
    let edgedRow = []
    let listOfEdged = []

    //Run the helper function starting from the top left pixel
    //Store the result in edged
    let edged = _EdgeDetect image threshold 0 0 width height edgedRow listOfEdged
    //To get the final result, reverse and horizontally flip the image
    let revEdge = List.rev edged
    let flipEdge = FlipHorizontal width height depth revEdge
    flipEdge
    
  //Function that rotates the Image by 90 degrees to the right    
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 

    //Transpose the entire image and set the result to transposed
    let transposed = List.transpose image
    //Reverse head and prepend it to the result of FlipHorizontal on tail
    match transposed with
    | [] -> [] //Base case
    | head::tail -> 
        let reversedList = List.rev head
        reversedList::FlipHorizontal width height depth (tail:(int*int*int) list list)
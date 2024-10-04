// F# image processing functions.
// Dua'a Hussein, 655469322, dhusse4
// UIC Spring 2024
// This program is meant to implement five different functions that imitate
// photo editting filters like sepia so that we can alter the images
// passed up as a PPM file
// It will manipulate the pixel values of the image tuples in order
// to imitate this filters and adjustments in a pseudo-type of way. 

namespace ImageLibrary

module Operations =
  // all functions must be indented
  // Sepia:
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //

  // Clamp function to ensure value stays within 0 to 255 range
  let clamp value = 
    if value < 0 then 0
    elif value > 255 then 255
    else value

  // Function to apply sepia filter to a single pixel
  let applySepiaToPixel (r, g, b) =
    let newRed = clamp (int (0.393 * float r + 0.769 * float g + 0.189 * float b))
    let newGreen = clamp (int (0.349 * float r + 0.686 * float g + 0.168 * float b))
    let newBlue = clamp (int (0.272 * float r + 0.534 * float g + 0.131 * float b))
    (newRed, newGreen, newBlue)
    
  // the actual function but uses the helper function above in order to execute fully
  let rec Sepia (width:int) 
                (height:int) 
                (depth:int) 
                (image:(int*int*int) list list) = 
    
    let applySepia (imageData : (int * int * int) list list) =
        List.map (fun row -> List.map applySepiaToPixel row) imageData

    applySepia image

  // Increase Intensity
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  // Returns: updated image.
  let rec IncreaseIntensity (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (intensity:double)
                    (channel:char) = 
    
    // Helper function to apply intensity change to a single pixel
    // call the clamp function from before to handle overflow
    let applyIntensityToPixel (r, g, b) =
        match channel with
        | 'r' -> (clamp (int (float r * intensity)), g, b)
        | 'g' -> (r, clamp (int (float g * intensity)), b)
        | 'b' -> (r, g, clamp (int (float b * intensity)))
        | _ -> (r, g, b) // If channel is not 'r', 'g', or 'b', leave pixel unchanged

    // Apply intensity change to each pixel in the image
    List.map (fun row -> List.map applyIntensityToPixel row) image


  // FlipHorizontal:
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  // Returns: updated image.
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    
    // Helper function to flip a single row horizontally
    // its better to do a single row at a time using the reverse function, 
    // so that it keeps the color value of each pixel while still changing its
    // position
    let flipRow (row : (int * int * int) list) =
        List.rev row

    // then we can apply horizontal flip to each row in the image
    List.map flipRow image

  // Rotate180:
  // Rotates the image 180 degrees.
  // Returns: updated image.
  let rec Rotate180 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    
    // Notes: rotate upside down => flip horizontally then left
    // can do this ==> reverse,  then reverse again? account for final
    // shift to turn upside down...
    // reverse each row's pixels, then take the entire image, reverse the rows of the image?
    // take two, broke apart each step so that its easier to see what went wrong 
    // after completion: OKAY i think my original method of going about it was just doing too 
    // much at once, and breaking it apart actually helped the content. 

    // reverse the content of a single row
    let reverseRowContent (row : (int * int * int) list) =
        // Reverses the order of elements in the row.
        List.rev row

    // reverse content to each row in the image
    let reversedContentImage = List.map reverseRowContent image

    // reverse the order of rows in the image
    let reversedImage = List.rev reversedContentImage

    // Return the final rotated image
    reversedImage

  // Edge Detection:
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  // Returns: updated image.
  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) = 
    
    // a helper function to calculate the distance between two pixels
    let pixelDistance (r1, g1, b1) (r2, g2, b2) =
        sqrt (float ((r1 - r2) * (r1 - r2) + (g1 - g2) * (g1 - g2) + (b1 - b2) * (b1 - b2)))

    // a helper function to check if an edge occurs for a pixel
    let checkEdge x y =
        let currentColor = List.nth (List.nth image y) x
        let rightColor = List.nth (List.nth image y) (x + 1)
        let bottomColor = List.nth (List.nth image (y + 1)) x
        let rightDistance = pixelDistance currentColor rightColor
        let bottomDistance = pixelDistance currentColor bottomColor
        if rightDistance > float threshold || bottomDistance > float threshold then
            (0, 0, 0) // Edge detected, return black
        else
            (255, 255, 255) // No edge detected, return white

    // Apply edge detection to each pixel in the image using map and mapi functions
    List.init (height - 1) (fun y ->
        List.init (width - 1) (fun x -> 
            checkEdge x y
        )
    )
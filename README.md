<p align="center">
  <img src="https://github.com/alwi0324/olahPeta/blob/master/man/olahPeta%20green.jpeg" alt="logo olahPeta" width="300">
</p>

# olahPeta <a href="https://github.com/alwi0324/olahPeta"><img src="https://img.shields.io/badge/version-1.0.0-blue.svg" alt="version"></a> <a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/license-MIT-green.svg" alt="license"></a>

`olahPeta` is an R package designed to automatically read, rename, and rotate (if necessary) multiple scanned BPS Census or Survey map images based on a 14-digit SLS code detected within each image using Optical Character Recognition (OCR).

<br>

## ✨ Features

- 🔥 **Fast scan**  
  Performs **OCR only on specific regions of the map** to read the area code, then automatically renames the file based on the detected code.
  It also detects and **corrects the image orientation** to ensure maps are not rotated incorrectly.

- ⚙️ **Handles thousands of files safely**  
  Uses `tryCatch()` and `gc()` to process large batches efficiently while keeping RAM usage under control.

- 🧠 **Smart JPG Filter**  
  Automatically skips non-JPG files and processes only JPG images.

- ⏱️ **Time Information**  
  Reports the total processing time (in minutes and seconds) for all renamed files.

- 📊 **Result Summary**  
  Displays the number of successfully renamed maps according to the specified regency/municipality code.

<br>

## 💾 Installation

Make sure devtools or remotes is already installed in your RStudio.

```r
install.packages("devtools")
devtools::install_github("alwi0324/olahPeta")
```
**Note:** If prompted to update certain packages (options like 1. All, 2. CRAN, etc.), simply press **ENTER** to skip. Wait until the installation process is complete and the message DONE (olahPeta) appears.

After installation, activate the package with the following code:
```r
library(olahPeta)
```

<br>

## ▶️ Usage

1. Make sure all scanned JPG maps are of good quality, with **no folds or creases**, especially **in the corner where the SLS code is located**.
2. Set working directory to the path that contains scanned JPG maps.  

   ```r
   setwd("path/to/your/scanned_maps")
3. Run this code to rename and rotate all scanned JPG maps in this directory. For example: 

   ```r
   rename_peta("5205")
   ```

   The string "5205" (or it can also numeric) represents the regency/municipality code, which corresponds to the first four digits of the SLS code. Once the process is complete, the log will display the number of maps successfully renamed based on this regency/municipality code.
4. Sit back and relax ☕ — the smart automation in olahPeta handles it all for you.

<br>

## 🖥️ Output
🎉 Here is the output for renaming 100 scanned map images.
![Log rename](https://github.com/alwi0324/olahPeta/blob/master/man/sukses%20rename.PNG)

<br>

![Hasil rename](https://github.com/alwi0324/olahPeta/blob/master/man/hasil%20rename.png)

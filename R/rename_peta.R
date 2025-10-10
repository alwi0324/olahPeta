## Helper function

#' @title Crop Image Corner for OCR
#' @description A helper function to crop a specific corner of an image,
#'   rotate it if necessary, and perform OCR to find a 14-digit code.
#' @param img An image object read by `magick::image_read()`.
#' @param corner The corner to crop. Can be one of "topleft", "topright",
#'   or "bottomright".
#' @return A character string of the 14-digit code if found, otherwise NULL.
#' @import magick
#' @importFrom tesseract tesseract ocr
#' @importFrom stringr str_detect
#' @importFrom purrr is_empty
#' @noRd
crop_corner <- function(img, corner = c("topleft", "topright", "bottomright")) {
  eng <- tesseract("eng")
  # pastikan orientasi gambar sesuai EXIF
  img <- image_orient(img)
  info <- image_info(img)
  w <- info$width
  h <- info$height

  corner <- match.arg(corner)

  # hitung offset berdasarkan sudut yang dipilih
  if (corner == "topleft") {
    geometry <- geometry_area(170, 700, 0, 0)
    cropped <- image_crop(img, geometry)
    cropped <- image_rotate(cropped, 90)
  } else if (corner == "topright") {
    geometry <- geometry_area(700, 170, w-700, 0)
    cropped <- image_crop(img, geometry)

  } else if (corner == "bottomright") {
    geometry <- geometry_area(170, 700, w-170, h-700)
    cropped <- image_crop(img, geometry)
    cropped <- image_rotate(cropped, -90)
  }

  # OCR aman dengan tryCatch
  text <- tryCatch(
    ocr(cropped, eng),
    error = function(e) return("")
  )

  text <- gsub("[§$]", "5", text)
  kode <- regmatches(text, regexpr("[0-9]{14}", text))

  # Bersihkan objek berat
  rm(cropped, img)
  invisible(gc())

  if (length(kode)) return(kode)
  else return(NULL)
}

#' @title Rename Scanned Map Files using OCR
#' @description This function automatically renames multiple JPEG map files in
#'   the current working directory based on a 14-digit SLS code found within
#'   the image.
#' @param kodekab A character string representing the district/municipality
#'   code to verify the renamed files.
#' @return The function is called for its side effect of renaming files.
#'   It also prints messages to the console about the progress and results.
#' @importFrom purrr is_empty
#' @export
#' @examples
#' \dontrun{
#' # To run this function, make sure you have .jpg files in your
#' # working directory.
#' # setwd("path/to/your/maps")
#' # rename_peta(kodekab = "3273")
#' }
rename_peta <- function(kodekab = NULL) {
  # Fungsi untuk mengekstrak kode dari gambar
  extract_kode_from_image <- function(image_object, ocr_engine) {
    text <- ocr(image_object, ocr_engine)
    text <- gsub("[§$]", "5", text)
    kode <- regmatches(text, regexpr("[0-9]{14}", text))
    return(kode)
  }
  sudut_rotasi <- c(0, 90, 180, 270) # Daftar sudut yang akan dicoba
  
  idsls <- c()
  files <- dir()
  files <- files[grepl("\\.jpg$", files, ignore.case = TRUE)]
  
  # Pastikan di folder ini berisi semua file jpg
  if (!is_empty(files)) {
    # kodekab harus ada
    if (!is.null(kodekab)) {
      # waktu mulai
      a <- Sys.time()
      
      for (i in 1:length(files)) {
        cat(paste0("Sedang membaca peta ke-",i," dari ",length(files), " peta\n"))
        
        # Reset engine setiap 30 file untuk menghindari error cache
        if (i %% 30 == 0) {
          invisible(gc())
          eng <<- tesseract("eng")
          message("🔄 Engine tesseract direset ulang.")
        }
        
        gbr <- image_orient(image_read(files[i]))
        info <- image_info(gbr)
        w <- info$width
        h <- info$height
        
        cat(paste0("Sedang mengambil kode SLS peta ke-",i,"\n"))
        
        if (w > h) { # gambar landscape
          kode <- crop_corner(gbr, "topright") # 1st try
          
          if (!is.null(kode)) {
            idsls <- c(idsls, kode)
            
            # jika tidak ada peta duplikat
            new_files <- substr(dir(), 1, nchar(dir())-4)
            if (length(which(new_files == kode)) == 0) {
              new_name <- paste0(kode, ".jpg")
            } else {
              # ada duplikat
              new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
            }
            file.rename(files[i], file.path(".", new_name))
            message("✅ Rename file berhasil: ", files[i], " -> ", new_name,"\n")
            
          } else {
            # jika null, putar gambarnya lalu rename
            putar <- image_rotate(gbr, 180)
            kode <- crop_corner(putar, "topright")
            
            if (!is.null(kode)) {
              idsls <- c(idsls, kode)
              
              # jika tidak ada peta duplikat
              new_files <- substr(dir(), 1, nchar(dir())-4)
              if (length(which(new_files == kode)) == 0) {
                new_name <- paste0(kode, ".jpg")
              } else {
                # ada duplikat
                new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
              }
              file.rename(files[i], file.path(".", new_name))
              image_write(putar, path = file.path(".", new_name), format = "jpg")
              message("✅ Rename dan putar file berhasil: ", files[i], " -> ", new_name,"\n")
              
              
            } else {
              # jika masih null, baca kode SLS di pojok kiri atas
              kode <- crop_corner(gbr, "topleft")
              
              if (!is.null(kode)) {
                idsls <- c(idsls, kode)
                
                # jika tidak ada peta duplikat
                new_files <- substr(dir(), 1, nchar(dir())-4)
                if (length(which(new_files == kode)) == 0) {
                  new_name <- paste0(kode, ".jpg")
                } else {
                  # ada duplikat
                  new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
                }
                file.rename(files[i], file.path(".", new_name))
                message("✅ Rename file berhasil: ", files[i], " -> ", new_name,"\n")
                
                
              } else {
                # jika masih null, putar dan baca kode SLS di pojok kiri atas
                putar <- image_rotate(gbr, 180)
                kode <- crop_corner(putar, "topleft")
                
                if(!is.null(kode)) {
                  idsls <- c(idsls, kode)
                  
                  # jika tidak ada peta duplikat
                  new_files <- substr(dir(), 1, nchar(dir())-4)
                  if (length(which(new_files == kode)) == 0) {
                    new_name <- paste0(kode, ".jpg")
                  } else {
                    # ada duplikat
                    new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
                  }
                  file.rename(files[i], file.path(".", new_name))
                  image_write(putar, path = file.path(".", new_name), format = "jpg")
                  message("✅ Rename dan putar file berhasil: ", files[i], " -> ", new_name,"\n")
                  
                  
                } else {
                  # baca seluruh gambar yang diputar
                  cat("Membaca seluruh gambar untuk mendapatkan kode SLS\n")
                  
                  # Variabel untuk melacak status
                  kode_ditemukan <- FALSE
                  
                  # Loop untuk setiap sudut rotasi
                  eng <- tesseract("eng")
                  for (sudut in sudut_rotasi) {
                    # Tentukan gambar mana yang akan diproses
                    # Jika sudut 0, gunakan gambar asli. Jika tidak, putar gambar.
                    if (sudut == 0) {
                      cat("Membaca gambar asli (0 derajat)...\n")
                      gambar_proses <- gbr
                    } else {
                      cat(paste0("Memutar dan membaca gambar (", sudut, " derajat)...\n"))
                      gambar_proses <- image_rotate(gbr, sudut)
                    }
                    
                    # Coba ekstrak kode dari gambar yang sedang diproses
                    kode <- extract_kode_from_image(gambar_proses, eng)
                    
                    # Jika kode berhasil ditemukan
                    if (!is_empty(kode)) {
                      new_name <- paste0(kode, ".jpg")
                      
                      # Ganti nama file asli
                      file.rename(files[i], file.path(".", new_name))
                      
                      # Jika gambar diputar (bukan gambar asli), simpan versi yang sudah diputar
                      if (sudut > 0) {
                        image_write(gambar_proses, path = file.path(".", new_name), format = "jpg")
                      }
                      
                      message("✅ Rename file berhasil: ", files[i], " -> ", new_name, "\n")
                      kode_ditemukan <- TRUE # Set status menjadi TRUE
                      break # Hentikan loop karena kode sudah ditemukan
                    }
                  }
                  
                  # Jika setelah semua rotasi dicoba dan kode tetap tidak ditemukan
                  if (!kode_ditemukan) {
                    message("❌ Gagal mengambil kode SLS dari file: ", files[i], ". Harap periksa file scan peta!\n")
                  }
                }
              }
            }
            
          }
          
        }
        else {
          # gambar portrait
          kode <- crop_corner(gbr, "topright") # 1st try
          
          if (!is.null(kode)) {
            idsls <- c(idsls, kode)
            
            # jika tidak ada peta duplikat
            new_files <- substr(dir(), 1, nchar(dir())-4)
            if (length(which(new_files == kode)) == 0) {
              new_name <- paste0(kode, ".jpg")
            } else {
              # ada duplikat
              new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
            }
            file.rename(files[i], file.path(".", new_name))
            message("✅ Rename file berhasil: ", files[i], " -> ", new_name,"\n")
            
          } else {
            # jika null, putar gambarnya lalu rename
            putar <- image_rotate(gbr, 180)
            kode <- crop_corner(putar, "topright")
            
            if (!is.null(kode)) {
              idsls <- c(idsls, kode)
              
              # jika tidak ada peta duplikat
              new_files <- substr(dir(), 1, nchar(dir())-4)
              if (length(which(new_files == kode)) == 0) {
                new_name <- paste0(kode, ".jpg")
              } else {
                # ada duplikat
                new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
              }
              file.rename(files[i], file.path(".", new_name))
              image_write(putar, path = file.path(".", new_name), format = "jpg")
              message("✅ Rename dan putar file berhasil: ", files[i], " -> ", new_name,"\n")
              
              
            } else {
              # jika masih null, baca kode SLS di pojok kanan bawah
              kode <- crop_corner(gbr, "bottomright")
              
              if (!is.null(kode)) {
                idsls <- c(idsls, kode)
                
                # jika tidak ada peta duplikat
                new_files <- substr(dir(), 1, nchar(dir())-4)
                if (length(which(new_files == kode)) == 0) {
                  new_name <- paste0(kode, ".jpg")
                } else {
                  # ada duplikat
                  new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
                }
                file.rename(files[i], file.path(".", new_name))
                message("✅ Rename file berhasil: ", files[i], " -> ", new_name,"\n")
                
                
              } else {
                # jika masih null, putar dan baca kode SLS di pojok kanan bawah
                putar <- image_rotate(gbr, 180)
                kode <- crop_corner(putar, "bottomright")
                
                if(!is.null(kode)) {
                  idsls <- c(idsls, kode)
                  
                  # jika tidak ada peta duplikat
                  new_files <- substr(dir(), 1, nchar(dir())-4)
                  if (length(which(new_files == kode)) == 0) {
                    new_name <- paste0(kode, ".jpg")
                  } else {
                    # ada duplikat
                    new_name <- paste0(kode, " (", length(which(idsls == kode)),").jpg")
                  }
                  file.rename(files[i], file.path(".", new_name))
                  image_write(putar, path = file.path(".", new_name), format = "jpg")
                  message("✅ Rename dan putar file berhasil: ", files[i], " -> ", new_name,"\n")
                  
                  
                } else {
                  # baca seluruh gambar yang diputar
                  cat("Membaca seluruh gambar untuk mendapatkan kode SLS\n")
                  
                  # Variabel untuk melacak status
                  kode_ditemukan <- FALSE
                  
                  # Loop untuk setiap sudut rotasi
                  eng <- tesseract("eng")
                  for (sudut in sudut_rotasi) {
                    # Tentukan gambar mana yang akan diproses
                    # Jika sudut 0, gunakan gambar asli. Jika tidak, putar gambar.
                    if (sudut == 0) {
                      cat("Membaca gambar asli (0 derajat)...\n")
                      gambar_proses <- gbr
                    } else {
                      cat(paste0("Memutar dan membaca gambar (", sudut, " derajat)...\n"))
                      gambar_proses <- image_rotate(gbr, sudut)
                    }
                    
                    # Coba ekstrak kode dari gambar yang sedang diproses
                    kode <- extract_kode_from_image(gambar_proses, eng)
                    
                    # Jika kode berhasil ditemukan
                    if (!is_empty(kode)) {
                      new_name <- paste0(kode, ".jpg")
                      
                      # Ganti nama file asli
                      file.rename(files[i], file.path(".", new_name))
                      
                      # Jika gambar diputar (bukan gambar asli), simpan versi yang sudah diputar
                      if (sudut > 0) {
                        image_write(gambar_proses, path = file.path(".", new_name), format = "jpg")
                      }
                      
                      message("✅ Rename file berhasil: ", files[i], " -> ", new_name, "\n")
                      kode_ditemukan <- TRUE # Set status menjadi TRUE
                      break # Hentikan loop karena kode sudah ditemukan
                    }
                  }
                  
                  # Jika setelah semua rotasi dicoba dan kode tetap tidak ditemukan
                  if (!kode_ditemukan) {
                    message("❌ Gagal mengambil kode SLS dari file: ", files[i], ". Harap periksa file scan peta!\n")
                  }
                }
              }
            }
            
          }
          
          
        }
      }
      
      # waktu selesai
      b <- Sys.time()
      
      berhasil <- length(which(str_detect(dir(), paste0("^",kodekab)) == T))
      if (berhasil > 0) {
        menit <- floor(time_length(b-a)/60)
        detik <- floor(time_length(b-a)%%60)
        if (menit == 0) {
          message(paste0("Durasi untuk rename file scan peta sebanyak ", length(files), " file adalah ", detik, " detik.\n"))
        } else {
          message(paste0("Durasi untuk rename file scan peta sebanyak ", length(files), " file adalah ", menit, " menit ", detik, " detik.\n"))
        }
        
        message("🎉 Rename peta selesai! Sebanyak ", berhasil, " file scan peta berhasil di-rename!")
      } else {
        message("❌ Tidak ada file peta yang bisa di-rename. Harap pastikan file scan peta tidak terlipat/kotak berisi kode SLS terbaca dengan jelas.")
      }
    } else {
      message("❌ Tidak ada kode kabupaten yang dimasukkan. Harap masukkan kode kabupaten Anda (contoh: \"3575\") \n")
      return(invisible(NULL)) # handle null argument kodekab
    }
  } else {
    message("❌ Tidak ada file JPG dalam folder ini. Silakan pindah ke direktori yang berisi file scan peta!\n")
  }
}

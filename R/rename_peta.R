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
#' @description Automatically renames and rotates multiple JPEG map files in the current
#'   working directory based on a 14-digit SLS code found within the image.
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
        
        # Reset engine setiap 50 file untuk menghindari error cache
        if (i %% 50 == 0) {
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

#' @title Organize Scanned Map Files
#' @description Organizing scanned map files based on the SLS codes
#'   and creating directories that correspond to each SLS code.
#' @param kodekab A character string representing the district/municipality
#'   code to filter files in this directory.
#' @param datawil A data frame (csv/geojson file) containing region information,
#'   typically including province, district/municipality, and village information.
#' @return The function is called for its side effect of organizing files.
#'   It also prints messages to the console about the progress and results.
#' @importFrom purrr is_empty
#' @importFrom data.table fread
#' @importFrom sf st_read
#' @export
#' @examples
#' \dontrun{
#' # Make sure your working directory contains scanned map files
#' # starting with kodekab.
#' # setwd("path/to/your/maps")
#' # org_peta(kodekab = "3273", datawil = "my final sls.geojson")
#' }
org_peta <- function(kodekab = NULL, datawil = NULL) {
  all_files <- dir() #ambil semua file di folder ini
  maps <- list.files(pattern = paste0("^", kodekab)) #ambil semua file berawalan kode kabkot
  
  # pastikan semua file berawalan kode kabkot
  if (!is_empty(maps)) {
    # kodekab harus ada
    if (!is.null(kodekab)) {
      kecs <- substr(maps, 1, 7) %>% unique()
      vils <- substr(maps, 1, 10) %>% unique()
      
      if (!is.null(datawil)) {
        maps_moved <- c()
        idwil <- st_read(datawil, quiet = T)
        Sys.sleep(1)
          
        for (i in 1:length(kecs)) {
          hasilKec <- filter(idwil, kdkec == substr(kecs[i],5,7))
          if (nrow(hasilKec)) {
            folderKec <- paste0("[", kodekab, hasilKec$kdkec[1], "] ", hasilKec$nmkec[1])
              
            # bikin folder kecamatan
            cat(paste0("Membuat folder ", folderKec, " di direktori ini\n"))
            dir.create(folderKec)
              
            # cek ketersediaan desa di maps yang ada di geojson
            this.vils <- vils[grepl(paste0("^", substr(kecs[i],1,7)), vils)]
            
            for (j in 1:length(this.vils)) {
              hasilDesa <- filter(hasilKec, kddesa == substr(this.vils[j],8,10))
              
              if (nrow(hasilDesa)) {
                folderDesa <- paste0("[", kodekab, hasilDesa$kdkec[1], hasilDesa$kddesa[1], "] ", hasilDesa$nmdesa[1])
                
                # bikin folder desa
                folderFinal <- paste0(folderKec, "/", folderDesa)
                cat(paste0("Membuat folder ", folderFinal, "\n"))
                dir.create(folderFinal, recursive = TRUE)
                
                # pindahkan peta ke folder desa yang sesuai
                maps.vils <- maps[grepl(paste0("^", this.vils[j]), maps)]
                maps_moved <- c(maps_moved, length(maps.vils))
                
                for (m in maps.vils) {
                  cat(paste0("Sedang memindahkan peta ", m, " ke dalam folder ", folderFinal,"\n"))
                  file.rename(m, paste0(folderFinal, "/", m))
                }
                rm(m)
                
              } else {
                message("❌ Tidak ditemukan desa dengan kode: ", this.vils[j]," di file geojson. Peta ini di-skip!") # desa ini di-skip
              }
            }
            cat("\n")
            rm(j)
            
          } else {
              message("❌ Tidak ditemukan kecamatan dengan kode: ", kecs[i]," di file geojson. Peta ini di-skip!") # kecamatan ini di-skip
          }
        }
        rm(i)
        
        message("\n🎉 Pemindahan file scan peta selesai. Sebanyak ", sum(maps_moved), " file scan peta berhasil dipindahkan ke folder yang sesuai!\n")
        
      } else {
        for (k in kecs) {
          cat(paste0("Membuat folder ", k, " di direktori ini\n"))
          dir.create(k)
          
          this.vils <- vils[grepl(paste0("^", k), vils)]
          
          for (v in this.vils) {
            cat(paste0("Membuat folder ", k, "/",v,"\n"))
            dir.create(paste0(k,"/",v))
            
            maps.vils <- maps[grepl(paste0("^", k, substr(v,8,10)), maps)]
            
            # pindahkan ke folder desa yang sesuai
            for (m in maps.vils) {
              cat(paste0("Sedang memindahkan peta ", m, " ke dalam folder ", k, "/", v,"\n"))
              file.rename(m, paste0(k, "/", v, "/", m))
            }
            rm(m)
          }
          cat("\n")
          rm(v)
        }
        rm(k)
        
        message("\n🎉 Pemindahan file scan peta selesai. Sebanyak ", length(maps), " file scan peta berhasil dipindahkan ke folder yang sesuai!\n")
      }
    } else {
      message("❌ Tidak ada kode kabupaten yang dimasukkan. Harap masukkan kode kabupaten Anda (contoh: \"3575\") \n")
      return(invisible(NULL)) # handle null argument kodekab
    }
  } else {
    message("❌ Tidak ada file scan peta yang berawalan kode kabupaten/kota: ", kodekab," di folder ini. Mohon masukkan kode kabupaten/kota yang sesuai!\n")
  }
  
}

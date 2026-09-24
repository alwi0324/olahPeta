#' @title Crop Image Corner for OCR
#' @description A helper function to crop an image in topright corner
#'   and perform OCR to find a 14-digit code.
#' @param img An image object read by `magick::image_read()`.
#' @return A character string of the 14-digit code if found, otherwise NULL.
#' @import magick
#' @importFrom tesseract tesseract ocr
#' @importFrom stringr str_detect
#' @importFrom purrr is_empty
#' @noRd
crop_corner <- function(img) {
  eng <- tesseract("eng")
  
  # Pastikan orientasi gambar sesuai EXIF
  img_orient <- image_orient(img)
  info <- image_info(img_orient)
  w <- info$width
  
  geometry <- geometry_area(700, 170, w - 700, 0)
  cropped <- image_crop(img_orient, geometry)
  
  # Percobaan 1: OCR Standar
  text <- tryCatch(
    ocr(cropped, eng),
    error = function(e) return("")
  )
  text <- gsub("[§$]", "5", text)
  kode <- regmatches(text, regexpr("[0-9]{16}|[0-9]{14}", text))
  
  # Percobaan 2: Jika kode kosong, pakai PSM 6 + Padding
  if (length(kode) == 0) {
    eng_crop <- tesseract("eng", options = list(tessedit_pageseg_mode = 6))
    cropped_padded <- image_border(cropped, "white", "20x20")
    
    text <- tryCatch(
      ocr(cropped_padded, eng_crop),
      error = function(e) return("")
    )
    text <- gsub("[§$]", "5", text)
    kode <- regmatches(text, regexpr("[0-9]{16}|[0-9]{14}", text))
  }
  
  rm(cropped, img_orient)
  invisible(gc())
  
  if (length(kode) > 0) {
    return(kode)
  } else {
    return(NULL)
  }
}

#' @title Rename Scanned Map Files using OCR
#' @description Automatically renames and rotates multiple JPEG map files in the current
#'   working directory based on a 14-digit SLS code found within the image.
#' @param kodekab A character string representing the municipality/regency
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
  if (is.null(kodekab)) {
    message("❌ Tidak ada kode kabupaten yang dimasukkan. Harap masukkan kode kabupaten Anda (contoh: '3575').\n")
    return(invisible(NULL))
  }
  
  files <- dir(pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)
  if (length(files) == 0) {
    message("❌ Tidak ada file scan peta (JPG/PNG) dalam folder ini.\n")
    return(invisible(NULL))
  }
  
  sudut_rotasi <- c(0, 180, 90, 270)
  idsls <- c()
  a <- Sys.time()
  
  # Helper OCR Fallback jika crop_corner gagal
  extract_kode_from_image <- function(image_object) {
    eng_crop <- tesseract("eng", options = list(tessedit_pageseg_mode = 6))
    image_padded <- image_border(image_object, "white", "20x20")
    
    text <- tryCatch(
      ocr(image_padded, eng_crop),
      error = function(e) return("")
    )
    text <- gsub("[§$]", "5", text)
    kode <- regmatches(text, regexpr("[0-9]{16}|[0-9]{14}", text))
    if (length(kode) > 0) return(kode) else return(NULL)
  }
  
  for (i in seq_along(files)) {
    cat(sprintf("\nSedang membaca peta ke-%d dari %d: %s\n", i, length(files), files[i]))
    
    if (i %% 30 == 0) {
      invisible(gc())
      message("🔄 Garbage collection dilakukan.")
    }
    
    # Bungkus tiap file agar error di 1 file tidak menghentikan seluruh proses
    tryCatch({
      gbr <- image_orient(image_read(files[i]))
      kode_ditemukan <- NULL
      gambar_final <- NULL
      
      # TAHAP 1: Coba 4 sudut rotasi ukuran asli dengan crop_corner
      for (sudut in sudut_rotasi) {
        gbr_tes <- if (sudut == 0) gbr else image_rotate(gbr, sudut)
        
        kode_ditemukan <- crop_corner(gbr_tes)
        if (!is.null(kode_ditemukan)) {
          gambar_final <- gbr_tes
          break
        }
      }
      
      # TAHAP 2: Jika masih gagal, naikkan resolusi (Resample 200 DPI dengan aman)
      if (is.null(kode_ditemukan)) {
        cat("Menaikkan resolusi gambar menjadi 200dpi...\n")
        
        info <- image_info(gbr)
        density_raw <- info$density
        
        # Ekstrasi DPI secara aman, fallback ke 96 jika NA/kosong
        img_dpi <- suppressWarnings(as.numeric(stringr::str_extract(density_raw, "^[0-9]+")))
        if (is.na(img_dpi) || img_dpi <= 0) {
          img_dpi <- 96 
        }
        
        scale_factor <- 200 / img_dpi
        if (scale_factor > 3) scale_factor <- 2.0 
        
        new_w <- round(info$width * scale_factor)
        new_h <- round(info$height * scale_factor)
        
        gbr_resampled <- image_resize(gbr, paste0(new_w, "x", new_h))
        
        # Coba lagi 4 sudut rotasi menggunakan full image OCR fallback
        for (sudut in sudut_rotasi) {
          gbr_tes <- if (sudut == 0) gbr_resampled else image_rotate(gbr_resampled, sudut)
          
          kode_ditemukan <- extract_kode_from_image(gbr_tes)
          if (!is.null(kode_ditemukan)) {
            gambar_final <- gbr_tes
            break
          }
        }
      }
      
      # TAHAP 3: Simpan / Rename File
      if (!is.null(kode_ditemukan)) {
        idsls <- c(idsls, kode_ditemukan)
        
        count_dup <- sum(idsls == kode_ditemukan)
        if (count_dup == 1) {
          new_name <- paste0(kode_ditemukan, ".jpg")
        } else {
          new_name <- paste0(kode_ditemukan, " (", count_dup, ").jpg")
        }
        
        image_write(gambar_final, path = file.path(".", new_name), format = "jpg")
        
        if (files[i] != new_name && file.exists(files[i])) {
          file.remove(files[i])
        }
        
        message("✅ Rename file berhasil: ", files[i], " -> ", new_name, "\n")
      } else {
        message("❌ Gagal mengambil kode SLS dari file: ", files[i], "\n")
      }
      
    }, error = function(e) {
      message("⚠️ Error memproses file ", files[i], ": ", e$message, "\n")
    })
  }
  
  # Laporan akhir
  b <- Sys.time()
  durasi <- as.numeric(difftime(b, a, units = "secs"))
  menit <- floor(durasi / 60)
  detik <- floor(durasi %% 60)
  
  berhasil <- sum(stringr::str_detect(dir(), paste0("^", kodekab)))
  
  if (menit == 0) {
    message(sprintf("Durasi untuk rename file scan peta sebanyak %d file adalah %d detik.\n", length(files), detik))
  } else {
    message(sprintf("Durasi untuk rename file scan peta sebanyak %d file adalah %d menit %d detik.\n", length(files), menit, detik))
  }
  
  if (berhasil > 0) {
    message(sprintf("🎉 Rename peta selesai! Sebanyak %d file scan peta berhasil di-rename!", berhasil))
  } else {
    message("❌ Tidak ada file peta yang bisa di-rename.")
  }
}

#' @title Organize Scanned Map Files
#' @description Organizing scanned map files based on the SLS codes
#'   and creating directories that correspond to each SLS code.
#' @param kodekab A character string representing the municipality/regency
#'   code to filter files in this directory.
#' @param datawil A data frame (geojson file) containing region information,
#'   typically including province, municipality/regency, district and village information.
#' @return The function is called for its side effect of organizing files.
#'   It also prints messages to the console about the progress and results.
#' @importFrom purrr is_empty
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


#' @title Check points outside their polygons
#' @description Check points outside its SLS polygon by iddesa.
#' @param ldmark A geojson file containing project name, project description, iddesa, project type, etc.
#' @param poly_map A data frame (geojson file) containing region information,
#'   typically including province, municipality/regency, district and village information.
#' @return The function is called for check points outside their polygons and will save.
#'   its result in Excel report.
#' @importFrom sf st_read
#' @importFrom sf st_intersects
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect
#' @importFrom writexl write_xlsx
#' @export
#' @examples
#' \dontrun{
#' # Make sure your working directory contains landmark and digital map files
#' # opa(ldmark = "my points.geojson", poly_map = "my final sls.geojson")
#' }
opa <- function(ldmark = NULL, poly_map = NULL) {
  if (!is.null(ldmark)) {
    if(!is.null(poly_map)) {

      if(str_detect(ldmark, "geojson") && str_detect(poly_map, "geojson")) {
        cat("Sedang membaca file geojson...\n")
        landmarks <- st_read(ldmark, quiet = T)
        sls <- st_read(poly_map, quiet = T)

        idvil <- unique(landmarks$iddesa)

        # filter desanya
        if (length(idvil) == 1) {
          vil_filter <- sls %>% filter(str_starts(idsls, idvil))
        } else {
          vil_filter <- sls %>% filter(str_starts(idsls, idvil[1]))
          for (i in 2:length(idvil)) {
            vil_filter <- rbind(vil_filter, sls %>% filter(str_starts(idsls, idvil[i])))
          }
          rm(i)
        }

        # Analisis titik
        idsls_awal <- vil_filter$idsls
        landmarks$idsls <- paste0(landmarks$iddesa, landmarks$nm_project)

        # hanya proses yang BUKAN pemekaran
        cat("Mengumpulkan idsls awal...\n")
        landmarks <- landmarks %>% filter(idsls %in% idsls_awal)
        idsls_ldmarks <- landmarks$idsls %>% unique()
        idx_sls <- c()

        for (l in idsls_ldmarks) {
          idx_sls <- c(idx_sls, which(vil_filter$idsls == l))
        }
        rm(l)

        # get number of points for each projects
        n_point <- c()
        for (i in idsls_ldmarks) {
          n_point <- c(n_point, length(which(landmarks$idsls == i)))
        }
        rm(i)

        # repeat idsls based on idx_sls and n_point
        idsls_rep <- c()
        for (i in 1:length(idx_sls)) {
          idsls_rep <- c(idsls_rep, rep(vil_filter$idsls[idx_sls[i]], times = n_point[i]))
        }
        rm(i)

        # get landmarks is inside/outside for each polygon
        cat("Sedang mengidentifikasi landmark berdasarkan poligon SLS...\n")
        hasil <- c()
        for (i in 1:nrow(landmarks)) {
          hasil <- c(hasil, st_intersects(landmarks[i,], vil_filter %>% filter(idsls == idsls_rep[i]), sparse = F)[1,])
        }

        # add to landmarks
        landmarks$is_inside <- hasil
        cat("Berhasil mengidentifikasi!\n")

        landmarks_final <- landmarks %>% arrange(nm_project) %>% filter(is_inside == FALSE) %>% select(idsls,
                                                                                                       nm_project,
                                                                                                       deskripsi_project,
                                                                                                       nama,
                                                                                                       tipe_landmark,
                                                                                                       user_creator_nama,
                                                                                                       user_created_at,
                                                                                                       user_upload_at,
                                                                                                       photo_url)

        landmarks_export <- st_drop_geometry(landmarks_final)
        cat("Sedang mengekspor landmark ke file excel...\n")

        # informasi waktu
        t <- as.character(Sys.time())
        t <- gsub(":", ".", unlist(strsplit(t, "\\."))[1])

        file_name <- paste0("landmark_outside_polygon_", paste(unique(landmarks$iddesa), collapse = "_"), "_", t, ".xlsx")
        write_xlsx(landmarks_export, file_name)
        Sys.sleep(2)
        if (file_name %in% dir()) {
          message("✅ Berhasil mengekspor file excel dengan nama: ", file_name,"\n")
        }


      } else {
        message("❗ Semua file harus berformat geojson!\n")
      }


    } else {
      message("❌ Tidak ada file peta digital dimasukkan. Harap masukkan file peta digital berformat geojson!\n")
      return(invisible(NULL)) # handle null poly_map
    }

  } else {
    message("❌ Tidak ada file landmark yang dimasukkan. Harap masukkan file landmark yang ingin dianalisis! \n")
    return(invisible(NULL)) # handle null argument ldmark
  }
}

#' @title Mengidentifikasi usaha/perusahaan hasil profiling SBR yang berada di luar desa
#' @description Mengidentifikasi usaha/perusahaan hasil profiling SBR yang berada di luar desa berdasarkan iddesa.
#' @param dir.titik Direktori file csv hasil scraping di website/aplikasi Matchapro.
#' @param dir.desa.sls Direktori file geojson peta desa/SLS terbaru (gunakan peta hasil Pemetaan Wilkerstat SE2026)
#'   untuk mengidentifikasi usaha-usaha/perusahaan-perusahaan yang berada di luar desa masing-masing.
#' @return Fungsi ini akan mengembalikan file excel dan geojson yang berisi
#'   usaha-usaha/perusahaan-perusahaan yang berada di luar desa masing-masing.
#' @importFrom sf st_read
#' @importFrom sf st_intersects
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_union
#' @importFrom sf st_write
#' @importFrom stringr str_detect
#' @importFrom writexl write_xlsx
#' @export
#' @examples
#' \dontrun{
#' # sbr_out_desa(dir.titik = "direktori_usaha_full_all_columns_2026.csv", dir.desa.sls = "Final_Desa_202415205.geojson")
#' }
sbr_out_desa <- function(dir.titik = NULL, dir.desa.sls = NULL, target = c("gc", "profiling")) {
  # Validasi parameter
  params <- c(dir.titik, dir.desa.sls)
  
  if (all(!is.null(params))) {
    # Load dataframe titik usaha
    if (str_detect(dir.titik, ".csv")) {
      titik <- read.csv(dir.titik)

      # cek titik di GC atau profiling
        keg <- match.arg(target)
      
      # Load geojson desa
      if (str_detect(dir.desa.sls, "geojson")) {
        desa <- st_read(dir.desa.sls, quiet = T)
        
        # cek apakah nama kolomnya mengandung iddesa
        if ("iddesa" %in% colnames(desa)) {
          cat("Sedang membaca file geojson desa...\n")
          if (keg == "gc") {
            file_name <- "Ground check_usaha_di_luar_desa_"
          } else {
            file_name <- "Profiling_usaha_di_luar_desa_"
          }
          Sys.sleep(2)
          desa <- desa %>% arrange(iddesa)
        } else {
          # poligonnya SLS
          cat("Sedang membaca file geojson SLS...\n")
          if (keg == "gc") {
            file_name <- "Ground check_usaha_di_luar_SLS_desa_"
          } else {
            file_name <- "Profiling_usaha_di_luar_SLS_desa_"
          }
          Sys.sleep(2)
          desa <- desa %>% arrange(idsls) %>% filter(str_starts(substr(idsls,11,14), "0")) %>% mutate(iddesa = substr(idsls,1,10))
          desa <- desa %>% st_make_valid() %>% group_by(iddesa, nmkec, nmdesa) %>% summarise(geometry = st_union(geometry))
        }
        
        if (keg == "gc") {
          # buang titik/usaha yang NA atau kosong
          titik.fix <- titik %>% filter(!is.na(gcs_result), !is.na(latitude_gc), gcs_result == 1, latlong_status_gc == "valid", nchar(kode_wilayah) == 10)
          
          # ubah ke df spasial
          st.titik.fix <- st_as_sf(titik.fix, coords = c("longitude_gc", "latitude_gc"), crs = 4326) %>% arrange(kode_wilayah)
        } else if (keg == "profiling") {
          # buang titik/usaha yang NA atau kosong
          titik.fix <- titik %>% filter(is.na(gcs_result), !is.na(latitude), latlong_status == "valid", status_perusahaan != "Duplikat", nchar(kode_wilayah) == 10)
          
          # ubah ke df spasial
          st.titik.fix <- st_as_sf(titik.fix, coords = c("longitude", "latitude"), crs = 4326) %>% arrange(kode_wilayah)
        }
        
        message(paste0("Ditemukan sebanyak ", nrow(st.titik.fix), " perusahaan yang memiliki titik koordinat.\n"))
        message("Akan memulai proses identifikasi titik koordinat perusahaan tersebut dalam 5 detik...\n\n")
        Sys.sleep(5.5)
        message("MULAI!\n")
        Sys.sleep(1)
        
        # get iddesa unik
        iddesa <- st.titik.fix$kode_wilayah %>% unique() %>% as.character()
        
        # Analisis titik
        is.inside <- sapply(1:nrow(st.titik.fix), function(i) {
          if (i %% 20 == 0 || i == nrow(st.titik.fix)) {
            cat(paste0("Sedang memproses perusahaan ke-",i," dari ", nrow(st.titik.fix), " perusahaan\n"))
          }
          id <- st.titik.fix$kode_wilayah[i]
          
          # Ambil poligon desa yang kodenya cocok
          poligon_target <- desa %>% filter(iddesa == id)
          
          # Cek interaksi (sparse = FALSE agar dpt TRUE/FALSE tunggal)
          res <- st_intersects(st.titik.fix$geometry[i], poligon_target$geometry, sparse = FALSE)
          
          # Jika ada hasil, ambil yang pertama, jika tidak ada poligon cocok beri FALSE
          if(length(res) > 0) return(res[1,1]) else return(FALSE)
        })
        
        message("\nSedang mengambil perusahaan yang titiknya di luar desa masing-masing...")
        luar.desa <- which(is.inside == F)
        hasil <- st.titik.fix[luar.desa,]
        
        # ambil kolom yang diperlukan
        if (keg == "gc") {
          hasilxl <- hasil %>% select(idsbr, nama_usaha, alamat_usaha, kode_wilayah, nmkec, nmdesa, kegiatan_usaha, status_perusahaan, history_ref_profiling_id, gc_username) %>% mutate(kegiatan_usaha = ifelse(kegiatan_usaha != '', str_extract(kegiatan_usaha, "(?<=\\[Kegiatan Usaha: ).*?(?=, Kategori)"), '')) %>% arrange(gc_username)
        } else if (keg == "profiling") {
          hasilxl <- hasil %>% select(idsbr, nama_usaha, alamat_usaha, kode_wilayah, nmkec, nmdesa, kegiatan_usaha, status_perusahaan, history_ref_profiling_id) %>% mutate(kegiatan_usaha = ifelse(kegiatan_usaha != '', str_extract(kegiatan_usaha, "(?<=\\[Kegiatan Usaha: ).*?(?=, Kategori)"), ''))
        }
        
        message(paste0("Ditemukan sebanyak ", nrow(hasil), " usaha yang berada di luar desanya\n"))
        message("Sedang mengekspor perusahaan yang di luar desa ke file excel dan geojson...\n")
        
        hasil.export <- st_drop_geometry(hasilxl)
        
        # informasi waktu
        t <- as.character(Sys.time())
        t <- gsub(":", ".", unlist(strsplit(t, "\\."))[1])
        
        writexl::write_xlsx(hasil.export, paste0(file_name, t, ".xlsx"))
        
        # save as geojson
        st_write(hasilxl, paste0(file_name, t, ".geojson"), quiet = TRUE)
        
        message("\n✅ Berhasil mengekspor file excel dengan nama: ", file_name, t, ".xlsx")
        message("✅ Berhasil mengekspor file geojson dengan nama: ", file_name, t, ".geojson")
        cat("Direktori hasil file ekspor:", getwd())
      } else {
        message("❗ File desa/SLS harus berformat geojson!\n")
      }
    } else {
      message("❗ File hasil scraping usaha harus berformat csv!\n")
    }
    
  } else {
    message("❌ Tentukan direktori usaha hasil scraping SBR dan direktori file geojson peta desa/SLS terlebih dahulu! \n")
    return(invisible(NULL))
  }
  
}

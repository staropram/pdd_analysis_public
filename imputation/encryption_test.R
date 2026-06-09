library(openssl)
library(getPass)

# Fixed salt and iteration count (shared with trusted collaborators)
# One-time generation (outside the script)
# rand_bytes(16)
# [1] 7a 93 4b 9c 00 55 b3 0d 6e d4 91 84 4a 3f 1c c2

.fixed_salt <- as.raw(c(
   0x7a, 0x93, 0x4b, 0x9c,
   0x00, 0x55, 0xb3, 0x0d,
   0x6e, 0xd4, 0x91, 0x84,
   0x4a, 0x3f, 0x1c, 0xc2
))
.iterations <- 100L

# --- Key Derivation ---
derive_key <- function(passphrase_raw) {
   bcrypt_pbkdf(passphrase_raw, salt = .fixed_salt, rounds=.iterations, size= 32L)
}

# --- Encrypt and save ---
encrypt_and_save <- function(object, filename) {
   passphrase <- charToRaw(getPass("Enter passphrase to encrypt: "))
   key <- derive_key(passphrase)
   serialized <- serialize(object, NULL)
   encrypted <- aes_cbc_encrypt(serialized, key = key)
   saveRDS(encrypted, filename)
   message("File encrypted and saved: ", filename)
}

# --- Load and decrypt ---
load_encrypted_file <- function(filename) {
   passphrase <- charToRaw(getPass("Enter passphrase to decrypt: "))
   key <- derive_key(passphrase)
   encrypted <- readRDS(filename)
   decrypted <- aes_cbc_decrypt(encrypted, key = key)
   unserialize(decrypted)
}

# --- Optional shortcut for one-file workflows ---
load_secret_data <- function(fn) {
   load_encrypted_file(fn)
}

save_secret_data <- function(object, fn) {
   encrypt_and_save(object, fn)
}


# Save it
save_secret_data(mtcars, "test.rds")

# Load it back
testLoaded <- load_secret_data("test.rds")
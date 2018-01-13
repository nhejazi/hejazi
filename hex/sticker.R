library(hexSticker)
library(here)

img_path <- here("hex", "nima_v2.png")
sticker_path <- here("hex", "nima_sticker.pdf")

sticker(img_path, package = "nima",
        p_color = "#242A30", p_size = 5, p_x = 1, p_y = 1.77,
        s_x = 1, s_y = 0.975, s_width = 0.6,
        h_fill = "#ffffff", h_color = "#1881C2",
        filename = sticker_path)


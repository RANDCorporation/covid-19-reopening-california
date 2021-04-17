library(hexSticker)


# Img from: https://raw.githubusercontent.com/github/explore/af16e074f93c080d7b283815787283cab0e9414b/topics/covid-19/covid-19.png
imgurl <- "inst/hex/covid-19.png"
sticker(imgurl,
        package="c19randepimod",
        p_size=15, p_y = 0.6,
        s_x=1, s_y=1.2,  # position
        s_width=0.5, s_height=0.45,
        h_fill = "#4B197C", h_color="white", p_color = "white",
        filename="inst/hex/c19randepimod-hex.png")

-- File type detection for Visual FoxPro files

vim.filetype.add({
  extension = {
    prg = "vfp",
    fxp = "vfp",
    spr = "vfp",
    mpr = "vfp",
    qpr = "vfp",
    vcx = "vfp",
    vct = "vfp",
    scx = "vfp",
    sct = "vfp",
    frx = "vfp",
    frt = "vfp",
    lbx = "vfp",
    lbt = "vfp",
    mnx = "vfp",
    mnt = "vfp",
  },
  pattern = {
    [".*%.prg"] = "vfp",
  },
})

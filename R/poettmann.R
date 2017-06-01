
poettmann_example <- function() {

    d = 1.380  # inches
    p1 = 500   # psig
    p2 = 1000  # psia
    t1 = 120   # deg F
    t2 = 150
    sgg = 0.65
    sgw = 1.07
    api = 22
    qo = 400   # STBD
    qw = 600
    ug = 0.018 # cp
    sigmao = 30 # dynes/cm
    sigmaw = 70
    glr = 500 # scf/stb

    ##### dummy values. Get these from the curves or correlations   ####
    z = 0.935
    Bo = 1.043
    Rs = 59
    ##########

    T = t1 + 460
    P1 = p1 + 14.7
    sgo = 141.5 / (131.5 + api)
    gor = (qw + qo) / qo * glr
    wor = qw / qo
    cat("wor:", wor)

    # determine mass per 1 STB, lbm/STB
    # m = wt oil + wt gas + wt water
    m =  350 * sgo + 0.0764 * sgg * gor + 350 * sgw * wor
    cat("mass:", m)

    # 4 total mal of produced fluid per day
    mt = qo * m
    cat(mt)

    # 5 start calculations of Bo and Rs at p1
    # 6 calculate total volume
    vol.oil = 5.61 * Bo
    vol.water = 5.61 * wor
    vol.freegas = gor - Rs
    vol.gas = vol.freegas * (14.7 / P1) * (T / 520) * (z / 1)
    vol.total = vol.oil + vol.water + vol.gas

    cat(vol.total)

    # 7 density at p1
    dens.mix = mt / vol.total

    # 8 flowing pressure gradient


}

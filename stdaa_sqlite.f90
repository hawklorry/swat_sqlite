      subroutine stdaa_sqlite

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual output to .std file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    basminpf    |kg P/ha       |final average amount of phosphorus in
!!                               |the mineral P pool in watershed soil
!!    basminpi    |kg P/ha       |initial average amount of phosphorus in
!!                               |the mineral P pool in watershed soil
!!    basno3f     |kg N/ha       |final average amount of nitrogen in the
!!                               |nitrate pool in watershed soil
!!    basno3i     |kg N/ha       |initial average amount of nitrogen in the
!!                               |nitrate pool in watershed soil
!!    basorgnf    |kg N/ha       |final average amount of nitrogen in the
!!                               |organic N pool in watershed soil
!!    basorgni    |kg N/ha       |initial average amount of nitrogen in the
!!                               |organic N pool in watershed soil
!!    basorgpf    |kg P/ha       |final average amount of phosphorus in
!!                               |the organic P pool in watershed soil
!!    basorgpi    |kg P/ha       |initial average amount of phosphorus in
!!                               |the organic P pool in watershed soil
!!    bio_aahv(:,:,:)|kg/ha         |harvested biomass of plant
!!    bio_aams(:) |metric tons/ha|average annual biomass (dry weight) in HRU
!!    cn2(:)      |none          |SCS runoff curve number for moisture
!!                               |condition II
!!    cpnm(:)     |NA            |four character code to represent crop name
!!    hru_km(:)   |km^2          |area of HRU in square kilometers
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    hruaao(1,:) |mm H2O        |precipitation in HRU during simulation
!!    hruaao(4,:) |mm H2O        |amount of surface runoff to main channel
!!                               |from HRU during simulation (ignores impact of
!!                               |transmission losses)
!!    hruaao(5,:) |mm H2O        |amount of lateral flow contribution to main
!!                               |channel from HRU during simulation
!!    hruaao(6,:) |mm H2O        |amount of groundwater flow contribution to
!!                               |main channel from HRU during simulation
!!    hruaao(12,:)|mm H2O        |actual evapotranspiration in HRU during
!!                               |simulation
!!    hruaao(14,:)|metric tons/ha|sediment yield from HRU for simulation
!!    hruaao(22,:)|mm H2O        |amount of irrigation water applied to HRU
!!                               |during simulation
!!    hruaao(28,:)|kg N/ha       |average annual amount of N (organic &
!!                               |mineral) auto-applied in HRU
!!    hruaao(29,:)|kg P/ha       |average annual amount of P (organic &
!!                               |mineral) auto-applied in HRU
!!    hruaao(35,:)|kg N/ha       |organic nitrogen in surface runoff in HRU
!!                               |during simulation
!!    hruaao(37,:)|kg N/ha       |nitrate in surface runoff in HRU during
!!                               |simulation
!!    hruaao(38,:)|kg N/ha       |nitrate in lateral flow in HRU during
!!                               |simulation
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idplt(:,:,:)|none          |land cover code from crop.dat
!!    ipot(:)     |none          |number of HRU (in subbasin) that is ponding
!!                               |water--the HRU that the surface runoff from
!!                               |current HRU drains into. This variable is
!!                               |used only for rice paddys or closed
!!                               |depressional areas
!!    irn(:)      |none          |average annual number of irrigation
!!                               |applications in HRU
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    mcr         |none          |max number of crops grown per year
!!    nhru        |none          |number of HRUs in watershed
!!    nro(:)      |none          |sequence number of year in rotation
!!    nrot(:)     |none          |number of years of rotation
!!    prog        |NA            |program name and version
!!    resdata(1)  |mm H2O        |average annual evaporation from reservoirs
!!                               |in watershed
!!    resdata(2)  |mm H2O        |average annual seepage from reservoirs in
!!                               |watershed
!!    resdata(3)  |mm H2O        |average annual precipitation on reservoirs
!!                               |in watershed
!!    resdata(4)  |mm H2O        |average annual amount of water transported
!!                               |into reservoirs in watershed
!!    resdata(5)  |metric tons/ha|average annual amount of sediment transported
!!                               |into reservoirs in watershed
!!    resdata(6)  |mm H2O        |average annual amount of water transported
!!                               |out of reservoirs in watershed
!!    resdata(7)  |metric tons/ha|average annual amount of sediment transported
!!                               |out of reservoirs in watershed
!!    sbactlchlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria lost from soil surface layer by
!!                               |percolation
!!    sbactlchp   |# colonies/ha |average annual number of persistent bacteria
!!                               |lost from soil surface layer by percolation
!!    sbactrolp   |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported to main channel
!!                               |with surface runoff in solution
!!    sbactrop    |# colonies/ha |average annual number of persistent bacteria
!!                               |transported to main channel with surface
!!                               |runoff in solution
!!    sbactsedlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported with sediment in
!!                               |surface runoff
!!    sdiegrolpq  |# colonies/ha |average annual change in the number of
!!                               |less persistent bacteria colonies in soil
!!                               |solution in watershed
!!    sdiegrolps  |# colonies/ha |average annual change in the number of
!!                               |less persistent bacteria colonies on soil
!!                               |particles in watershed
!!    sdiegropq   |# colonies/ha |average annual change in the number of
!!                               |persistent bacteria colonies in soil solution
!!                               |in watershed
!!    sdiegrops   |# colonies/ha |average annual change in the number of
!!                               |persistent bacteria colonies on soil particles
!!                               |in watershed
!!    snam(:)     |NA            |soil series name
!!    sno3up      |kg N/ha       |amount of nitrate moving upward in the soil
!!                               |profile in watershed
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    spadyev     |mm H2O        |average annual amount of water removed
!!                               |from potholes by evaporation in watershed
!!    spadyo      |mm H2O        |average annual amount of water released to
!!                               |main channel from potholes in watershed
!!    spadyrfv    |mm H2O        |average annual amount of precipitation on
!!                               |potholes in watershed
!!    spadysp     |mm H2O        |average annual amount of water removed
!!                               |from potholes by seepage in watershed
!!    sumix(:)    |none          |sum of mixing efficiencies in HRU
!!    title       |NA            |title from file.cio
!!    usle_ls(:)  |none          |USLE equation length slope (LS) factor
!!    wshd_aamon(:,1)|mm H2O        |average annual precipitation in watershed
!!                               |falling during month
!!    wshd_aamon(:,2)|mm H2O        |average annual freezing rain in watershed
!!                               |falling during month
!!    wshd_aamon(:,3)|mm H2O        |average annual surface runoff in watershed
!!                               |during month
!!    wshd_aamon(:,4)|mm H2O        |average annual lateral flow in watershed
!!                               |during month
!!    wshd_aamon(:,5)|mm H2O        |average annual water yield in watershed
!!                               |during month
!!    wshd_aamon(:,6)|mm H2O        |average annual actual evapotranspiration
!!                               |in watershed during month
!!    wshd_aamon(:,7)|metric tons   |average annual sediment yield in watershed
!!                               |during month
!!    wshd_aamon(:,8)|mm H2O        |average annual potential evapotranspiration
!!                               |in watershed during month
!!    wshd_dnit   |kg N/ha       |average annual amount of nitrogen lost from
!!                               |nitrate pool due to denitrification in
!!                               |watershed
!!    wshd_fixn   |kg N/ha       |average annual amount of nitrogen added to
!!                               |plant biomass via fixation
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_forgn  |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_forgp  |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    wshd_hmn    |kg N/ha       |average annual amount of nitrogen moving
!!                               |from active organic to nitrate pool in
!!                               |watershed
!!    wshd_hmp    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from organic to labile pool in watershed
!!    wshd_nitn   |kg N/ha       |average annual amount of nitrogen moving
!!                               |from the NH3 to the NO3 pool by
!!                               |nitrification in the watershed
!!    wshd_nstrs  |stress units  |average annual number of nitrogen stress
!!                               |units in watershed
!!    wshd_pal    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from labile mineral to active mineral pool
!!                               |in watershed
!!    wshd_pas    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from active mineral to stable mineral pool
!!                               |in watershed
!!    wshd_plch   |kg P/ha       |average annual amount of phosphorus leached
!!                               |into second soil layer
!!    wshd_pstrs  |stress units  |average annual number of phosphorus stress
!!                               |units in watershed
!!    wshd_pup    |kg P/ha       |average annual amount of plant uptake of
!!                               |phosphorus
!!    wshd_raino3 |kg N/ha       |average annual amount of NO3 added to soil
!!                               |by rainfall in watershed
!!    wshd_rmn    |kg N/ha       |average annual amount of nitrogen moving
!!                               |from fresh organic (residue) to nitrate
!!                               |and active organic pools in watershed
!!    wshd_rmp    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from fresh organic (residue) to labile
!!                               |and organic pools in watershed
!!    wshd_rwn    |kg N/ha       |average annual amount of nitrogen moving
!!                               |from active organic to stable organic pool
!!                               |in watershed
!!    wshd_tstrs  |stress units  |average annual number of temperature stress
!!                               |units in watershed
!!    wshd_voln   |kg N/ha       |average annual amount if nitrogen lost by
!!                               |ammonia volatilization in watershed
!!    wshd_wstrs  |stress units  |average annual number of water stress units
!!                               |in watershed
!!    wshd_yldn   |kg N/ha       |amount of nitrogen removed from soil in
!!                               |watershed in the yield
!!    wshd_yldp   |kg P/ha       |amount of phosphorus removed from soil in
!!                               |watershed in the yield
!!    wshdaao(1)  |mm H2O        |average amount of precipitation in watershed
!!                               |for the simulation
!!    wshdaao(3)  |mm H2O        |surface runoff in watershed for simulation
!!    wshdaao(4)  |mm H2O        |lateral flow contribution to streamflow in
!!                               |watershed for simulation
!!    wshdaao(5)  |mm H2O        |water percolation past bottom of soil profile
!!                               |in watershed for simulation
!!    wshdaao(6)  |mm H2O        |water yield to streamflow from HRUs in
!!                               |watershed for simulation
!!    wshdaao(7)  |mm H2O        |actual evapotranspiration in watershed
!!                               |for simulation
!!    wshdaao(11) |metric tons/ha|net change in sediment of reservoirs in
!!                               |watershed during simulation
!!    wshdaao(12) |metric tons/ha|sediment yield from HRUs in watershed for
!!                               |the simulation
!!    wshdaao(13) |metric tons/ha|sediment loading to ponds in watershed 
!!                               |during simulation
!!    wshdaao(14) |metric tons/ha|sediment loading from ponds in watershed
!!                               |during simulation
!!    wshdaao(15) |metric tons/ha|net change in sediment level in ponds in
!!                               |watershed during simulation
!!    wshdaao(19) |mm H2O        |evaporation from ponds in watershed during
!!                               |simulation
!!    wshdaao(20) |mm H2O        |seepage from ponds in watershed during
!!                               |simulation
!!    wshdaao(21) |mm H2O        |precipitation on ponds in watershed during
!!                               |simulation
!!    wshdaao(22) |mm H2O        |volume of water entering ponds in watershed
!!                               |during simulation
!!    wshdaao(23) |mm H2O        |volume of water leaving ponds in watershed
!!                               |during simulation
!!    wshdaao(33) |mm H2O        |net change in water volume of ponds in
!!                               |watershed during simulation
!!    wshdaao(34) |mm H2O        |net change in water volume of reservoirs in
!!                               |watershed during simulation
!!    wshdaao(36) |mm H2O        |snow melt in watershed for simulation
!!    wshdaao(38) |mm H2O        |average amount of tributary channel
!!                               |transmission losses in watershed during
!!                               |simulation
!!    wshdaao(39) |mm H2O        |freezing rain/snow fall in watershed for 
!!                               |the simulation
!!    wshdaao(40) |kg N/ha       |organic N loading to stream in watershed for
!!                               |the simulation
!!    wshdaao(41) |kg P/ha       |organic P loading to stream in watershed for
!!                               |the simulation
!!    wshdaao(42) |kg N/ha       |nitrate loading to stream in surface runoff
!!                               |in watershed for the simulation
!!    wshdaao(43) |kg P/ha       |soluble P loading to stream in watershed for
!!                               |the simulation
!!    wshdaao(44) |kg N/ha       |plant uptake of N in watershed for the 
!!                               |simulation
!!    wshdaao(45) |kg N/ha       |nitrate loading to stream in lateral flow
!!                               |in watershed for the simulation
!!    wshdaao(46) |kg N/ha       |nitrate percolation past bottom of soil
!!                               |profile in watershed for the simulation
!!    wshdaao(104)|mm H2O        |groundwater contribution to stream in
!!                               |watershed for the simulation (shallow aquifer)
!!    wshdaao(105)|mm H2O        |amount of water moving from shallow aquifer
!!                               |to plants/soil profile in watershed during
!!                               |simulation
!!    wshdaao(106)|mm H2O        |deep aquifer recharge in watershed during
!!                               |simulation
!!    wshdaao(107)|mm H2O        |total amount of water entering both aquifers
!!                               |in watershed during simulation
!!    wshdaao(108)|mm H2O        |potential evapotranspiration in watershed
!!                               |for the simulation
!!    wshdaao(109)|mm H2O        |drainage tile flow contribution to stream
!!                               |in watershed for the simulation
!!    wshdaao(113)|mm H2O        |groundwater contribution to stream in
!!                               |watershed for the simulation (deep aquifer)
!!    yldaa(:)    |metric tons/ha|average annual yield (dry weight) in HRU
!!    yldn(:,:,:) |kg/ha         |average value for yield of crop
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |counter
!!    k           |none          |counter
!!    nicr        |none          |sequence number for crop in year
!!    nnro        |none          |sequence number for year in rotation
!!    sumpady     |none          |number of HRUs with potholes
!!    xirr        |mm H2O        |average annual amount of irrigation water
!!                               |applied to watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
 
      real :: xirr
      integer :: j, nnro, nicr, k, sumpady, ncrp
      integer :: colnum, maxcropnum
      character*30 tblname
      character*4 cropname
      character*6 acp_crp, acp_yld, acp_bio

        xirr = 0.
        xirr = Sum(aairr)
        !! print irrigation data
        if (xirr > 0.) then
          !!create table ave_annual_irrigation
          tblname = 'ave_annual_irrigation'
          colnum = 3
          allocate(colair(colnum))
          call sqlite3_column_props( colair(1), "HRU", SQLITE_INT)
          call sqlite3_column_props( colair(2), "NO_APP", SQLITE_INT)
          call sqlite3_column_props( colair(3), "VOL_APP_MM", SQLITE_REAL)
          call sqlite3_create_table( db, tblname, colair )

          !!write data
          do j = 1, nhru
            call sqlite3_set_column( colair(1), j )
            call sqlite3_set_column( colair(2), irn(j) )
            call sqlite3_set_column( colair(3), aairr(j) )
            call sqlite3_insert( db, tblname, colair )
          end do
        end if
     
!! write average crop information to std output file
!      ncrp = 0
!      if (mcr < 3) then
      ncrp = mcr
!      else
!        ncrp = 3
!      end if

      !!find max number of crops in all hru
      maxcropnum = 0
      do j = 1, nhru
        if(maxcropnum < mcrhru(j)) maxcropnum = mcrhru(j)
      end do

      !!create table ave_plant
      tblname = 'ave_plant'
      colnum = maxcropnum * 3
      allocate(colacp(2 + colnum))
      call sqlite3_column_props( colacp(1), "SUB", SQLITE_INT)
      call sqlite3_column_props( colacp(2), "HRU", SQLITE_INT)
      do j = 1, maxcropnum
        write(acp_crp,"(a,i3.3)") "CRP", j
        write(acp_yld,"(a,i3.3)") "YLD", j
        write(acp_bio,"(a,i3.3)") "BIO", j
        call sqlite3_column_props( colacp(3+(j-1)*3), acp_crp, SQLITE_CHAR,4)
        call sqlite3_column_props( colacp(4+(j-1)*3), acp_yld, SQLITE_REAL)
        call sqlite3_column_props( colacp(5+(j-1)*3), acp_bio, SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblname, colacp )

      !!write data
      do j = 1, nhru
          call sqlite3_set_column( colacp(1), hru_sub(j) )
          call sqlite3_set_column( colacp(2), j )
          do nicr = 1, maxcropnum
            if(nicr <= mcrhru(j)) then
                call sqlite3_set_column( colacp(3+(nicr-1)*3), cpnm(idplrot(nicr,j)) )
                call sqlite3_set_column( colacp(4+(nicr-1)*3), yldn(nicr,j) )
                call sqlite3_set_column( colacp(5+(nicr-1)*3), bio_aahv(nicr,j) )
            else
                call sqlite3_set_column( colacp(3+(nicr-1)*3), "BARE" )
                call sqlite3_set_column( colacp(4+(nicr-1)*3), 0. )
                call sqlite3_set_column( colacp(5+(nicr-1)*3), 0. )
            end if
          end do
          call sqlite3_insert( db, tblname, colacp )
      end do

!! write average annual HRU data

      !!create table ave_annual_hru
      tblname = 'ave_annual_hru'
      colnum = size(hedahu)
      allocate(colahu(4+colnum))
      call sqlite3_column_props( colahu(1), "SUB", SQLITE_INT)
      call sqlite3_column_props( colahu(2), "HRU", SQLITE_INT)
      call sqlite3_column_props( colahu(3), "CPMN", SQLITE_CHAR,4)
      call sqlite3_column_props( colahu(4), "SOIL", SQLITE_CHAR,16)
      do j = 1, colnum
        call sqlite3_column_props( colahu(4+j), hedahu(j), SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblname, colahu )

      !!write data
      do j = 1, nhru
          if (idplt(j) > 0) then
            cropname = cpnm(idplt(j))
          else
            cropname = 'BARE'
          end if
          call sqlite3_set_column( colahu(1), hru_sub(j) )
          call sqlite3_set_column( colahu(2), j )
          call sqlite3_set_column( colahu(3), cropname )
          call sqlite3_set_column( colahu(4), snam(j) )
          call sqlite3_set_column( colahu(5), hru_km(j) )
          call sqlite3_set_column( colahu(6), cn2(j) )
          call sqlite3_set_column( colahu(7), sol_sumfc(j) )
          call sqlite3_set_column( colahu(8), usle_ls(j) )
          call sqlite3_set_column( colahu(9),hruaao(22,j)  )
          call sqlite3_set_column( colahu(10),hruaao(28,j)  )
          call sqlite3_set_column( colahu(11),hruaao(29,j)  )
          call sqlite3_set_column( colahu(12),sumix(j)  )
          call sqlite3_set_column( colahu(13),hruaao(1,j)  )
          call sqlite3_set_column( colahu(14),hruaao(19,j)  )
          call sqlite3_set_column( colahu(15),hruaao(5,j) + hruaao(6,j))
          call sqlite3_set_column( colahu(16),hruaao(12,j)  )
          call sqlite3_set_column( colahu(17),hruaao(14,j)  )
          call sqlite3_set_column( colahu(18),hruaao(37,j)+hruaao(38,j))
          call sqlite3_set_column( colahu(19),hruaao(35,j)  )
          call sqlite3_set_column( colahu(20),bio_aams(j)  )
          call sqlite3_set_column( colahu(21),yldaa(j)  )
          call sqlite3_set_column( colahu(22),hruaao(4,j) )
          call sqlite3_insert( db, tblname, colahu )
      end do

!! write average annual watershed monthly values

      !!create table ave_monthly_basin
      tblname = 'ave_monthly_basin'
      colnum = size(hedamo)
      allocate(colamo(1+colnum))
      call sqlite3_column_props( colamo(1), "MO", SQLITE_INT)
      do j = 1, colnum
        call sqlite3_column_props( colamo(1+j), hedamo(j), SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblname, colamo )

      !!write data
      do j = 1, 12
        call sqlite3_set_column( colamo(1), j )
        do k = 1, 8
          call sqlite3_set_column( colamo(1+k), wshd_aamon(j,k) )
        end do
        call sqlite3_insert( db, tblname, colamo )
      end do

!! create table ave_annual_basin for any single value result in basin level
      tblname = 'ave_annual_basin'
      write(tblabn,"(a)") tblname
      colnum = 2
      allocate(colabn(colnum))
      call sqlite3_column_props( colabn(1), "NAME", SQLITE_CHAR,50)
      call sqlite3_column_props( colabn(2), "VALUE", SQLITE_REAL)
      call sqlite3_create_table( db, tblname, colabn )

!! write average annual stress values
      call stdaa_sqlite_writebasin("WATER STRESS DAYS",wshd_wstrs)
      call stdaa_sqlite_writebasin("TEMPERATURE STRESS DAYS",wshd_tstrs)
      call stdaa_sqlite_writebasin("NITROGEN STRESS DAYS",wshd_nstrs)
      call stdaa_sqlite_writebasin("PHOSPHORUS STRESS DAYS",wshd_pstrs)
      call stdaa_sqlite_writebasin("AERATION STRESS DAYS",wshd_astrs)

!! watershed summary water balance table
      call stdaa_sqlite_writebasin("PRECIP MM",wshdaao(1))
      call stdaa_sqlite_writebasin("SNOW FALL MM",wshdaao(39))
      call stdaa_sqlite_writebasin("SNOW MELT MM",wshdaao(36))
      call stdaa_sqlite_writebasin("SUBLIMATION MM",wshdaao(37))
      call stdaa_sqlite_writebasin("SURFACE RUNOFF Q MM",wshdaao(3))
      call stdaa_sqlite_writebasin("LATERAL SOIL Q MM",wshdaao(4))
      call stdaa_sqlite_writebasin("TILE Q MM",wshdaao(109))
      call stdaa_sqlite_writebasin("GROUNDWATER (SHAL AQ) Q MM",wshdaao(104))
      call stdaa_sqlite_writebasin("GROUNDWATER (DEEP AQ) Q MM",wshdaao(113))
      call stdaa_sqlite_writebasin("REVAP (SHAL AQ => SOIL/PLANTS) MM",wshdaao(105))
      call stdaa_sqlite_writebasin("DEEP AQ RECHARGE MM",wshdaao(106))
      call stdaa_sqlite_writebasin("TOTAL AQ RECHARGE MM",wshdaao(107))
      call stdaa_sqlite_writebasin("TOTAL WATER YLD MM",wshdaao(6))
      call stdaa_sqlite_writebasin("PERCOLATION OUT OF SOIL MM",wshdaao(5))
      call stdaa_sqlite_writebasin("ET MM",wshdaao(7))
      call stdaa_sqlite_writebasin("PET MM",wshdaao(108))
      call stdaa_sqlite_writebasin("TRANSMISSION LOSSES MM",wshdaao(38))
      call stdaa_sqlite_writebasin("SEPTIC INFLOW MM",wshd_sepmm)
      call stdaa_sqlite_writebasin("TOTAL SEDIMENT LOADING T/HA",wshdaao(12))

!! watershed pothole summary values
!      sumpady = 0
!     sumpady = Sum(ipot)
!      if (sumpady > 0) then
      call stdaa_sqlite_writebasin("TILE FROM IMPOUNDED WATER MM",spadyo)
      call stdaa_sqlite_writebasin("EVAPORATION FROM IMPOUNDED WATER MM",spadyev)
      call stdaa_sqlite_writebasin("SEEPAGE INTO SOIL FROM IMPOUNDED WATER MM",spadysp)
      call stdaa_sqlite_writebasin("OVERFLOW FROM IMPOUNDED WATER MM",spadyosp)

!     end if

!! watershed summary nutrient table
      !!format 2700
      call stdaa_sqlite_writebasin("ORGANIC N(KG/HA)",wshdaao(40))
      call stdaa_sqlite_writebasin("ORGANIC P(KG/HA)",wshdaao(41))
      call stdaa_sqlite_writebasin("NO3 YIELD (SQ)(KG/HA)",wshdaao(42))
      call stdaa_sqlite_writebasin("NO3 YIELD (LAT)(KG/HA)",wshdaao(45))
      call stdaa_sqlite_writebasin("NO3 YIELD (TILE)(KG/HA)",wshdaao(111))
      call stdaa_sqlite_writebasin("SOLP YIELD (TILE)(KG/HA)",wshd_ptile)
      call stdaa_sqlite_writebasin("SOLP YIELD (SURF INLET RISER)(KG/HA)",wshd_pinlet)
      call stdaa_sqlite_writebasin("SOL P YIELD(KG/HA)",wshdaao(43))
      call stdaa_sqlite_writebasin("NO3 LEACHED(KG/HA)",wshdaao(46))
      call stdaa_sqlite_writebasin("P LEACHED(KG/HA)",wshd_plch)
      call stdaa_sqlite_writebasin("N UPTAKE(KG/HA)",wshdaao(44))
      call stdaa_sqlite_writebasin("P UPTAKE(KG/HA)",wshd_pup)
      call stdaa_sqlite_writebasin("NO3 YIELD (GWQ)(KG/HA)",wshdaao(110))

      !!format 2800
      call stdaa_sqlite_writebasin("ACTIVE TO SOLUTION P FLOW(KG/HA)",wshd_pal)
      call stdaa_sqlite_writebasin("ACTIVE TO STABLE P FLOW(KG/HA)",wshd_pas)
      call stdaa_sqlite_writebasin("N FERTILIZER APPLIED(KG/HA)",wshd_ftotn)
      call stdaa_sqlite_writebasin("P FERTILIZER APPLIED(KG/HA)",wshd_ftotp)
      call stdaa_sqlite_writebasin("N FIXATION(KG/HA)",wshd_fixn)
      call stdaa_sqlite_writebasin("DENITRIFICATION(KG/HA)",wshd_dnit)

      !!format 2900
      call stdaa_sqlite_writebasin("HUMUS MIN ON ACTIVE ORG N(KG/HA)",wshd_hmn)
      call stdaa_sqlite_writebasin("ACTIVE TO STABLE ORG N(KG/HA)",wshd_rwn)
      call stdaa_sqlite_writebasin("HUMUS MIN ON ACTIVE ORG P(KG/HA)",wshd_hmp)
      call stdaa_sqlite_writebasin("MIN FROM FRESH ORG N(KG/HA)",wshd_rmn)
      call stdaa_sqlite_writebasin("MIN FROM FRESH ORG P(KG/HA)",wshd_rmp)

      !!format 3000
      call stdaa_sqlite_writebasin("NO3 IN RAINFALL(KG/HA)",wshd_raino3)
      call stdaa_sqlite_writebasin("INITIAL NO3 IN SOIL(KG/HA)",basno3i)
      call stdaa_sqlite_writebasin("FINAL NO3 IN SOIL(KG/HA)",basno3f)
      call stdaa_sqlite_writebasin("INITIAL ORG N IN SOIL(KG/HA)",basorgni)
      call stdaa_sqlite_writebasin("FINAL ORG N IN SOIL(KG/HA)",basorgnf)
      call stdaa_sqlite_writebasin("INITIAL MIN P IN SOIL(KG/HA)",basminpi)
      call stdaa_sqlite_writebasin("FINAL MIN P IN SOIL(KG/HA)",basminpf)
      call stdaa_sqlite_writebasin("INITIAL ORG P IN SOIL(KG/HA)",basorgpi)
      call stdaa_sqlite_writebasin("FINAL ORG P IN SOIL(KG/HA)",basorgpf)
      call stdaa_sqlite_writebasin("NO3 IN FERT(KG/HA)",wshd_fno3)
      call stdaa_sqlite_writebasin("AMMONIA IN FERT(KG/HA)",wshd_fnh3)
      call stdaa_sqlite_writebasin("ORG N IN FERT(KG/HA)",wshd_forgn)
      call stdaa_sqlite_writebasin("MINERAL P IN FERT(KG/HA)",wshd_fminp)
      call stdaa_sqlite_writebasin("ORG P IN FERT(KG/HA)",wshd_forgp)
      call stdaa_sqlite_writebasin("N REMOVED IN YIELD(KG/HA)",wshd_yldn)
      call stdaa_sqlite_writebasin("P REMOVED IN YIELD(KG/HA)",wshd_yldp)
      call stdaa_sqlite_writebasin("AMMONIA VOLATILIZATION(KG/HA)",wshd_voln)
      call stdaa_sqlite_writebasin("AMMONIA NITRIFICATION(KG/HA)",wshd_nitn)
      call stdaa_sqlite_writebasin("NO3 EVAP-LAYER 2 TO 1",sno3up)

!! watershed bacteria summary table
      !!format 3100
      call stdaa_sqlite_writebasin("DIE-GRO P Q (No/HA)",sdiegropq)
      call stdaa_sqlite_writebasin("DIE-GRO LP Q (No/HA)",sdiegrolpq)
      call stdaa_sqlite_writebasin("DIE-GRO P SED(No/HA)",sdiegrops)
      call stdaa_sqlite_writebasin("DIE-GRO LP SED(No/HA)",sdiegrolps)
      call stdaa_sqlite_writebasin("BACT P RUNOFF(No/HA)",sbactrop)
      call stdaa_sqlite_writebasin("BACT LP RUNOFF(No/HA)",sbactrolp)
      call stdaa_sqlite_writebasin("BACT P SEDIMENT(No/HA)",sbactsedp)
      call stdaa_sqlite_writebasin("BACT LP SEDIMENT(No/HA)",sbactsedlp)
      call stdaa_sqlite_writebasin("BACT P INCORP(No/HA)",sbactlchp)
      call stdaa_sqlite_writebasin("BACT LP INCORP(No/HA)",sbactlchlp)

!! septic variables to output.std
      !!format 3101
      call stdaa_sqlite_writebasin("NITRATE SEPTIC(kg/ha)",wshd_sepno3)
      call stdaa_sqlite_writebasin("AMMONIA SEPTIC(kg/ha)",wshd_sepnh3)
      call stdaa_sqlite_writebasin("ORG N SEPTIC(kg/ha)",wshd_seporgn)
      call stdaa_sqlite_writebasin("FRESH ORGN SEPTIC(kg/ha)",wshd_sepfon)
      call stdaa_sqlite_writebasin("ORG P SEPTIC(kg/ha)",wshd_seporgp)
      call stdaa_sqlite_writebasin("FRESH ORGP SEPTIC(kg/ha)",wshd_sepfop)
      call stdaa_sqlite_writebasin("SOL P SEPTIC(kg/ha)",wshd_sepsolp)
      call stdaa_sqlite_writebasin("BOD SEPTIC (kg/ha)",wshd_sepbod)

!!Add following information to help interface
      call stdaa_sqlite_writebasin("START_YEAR",real(iyr - nbyr))
      call stdaa_sqlite_writebasin("START_YEAR_OUTPUT",real(iyr - nbyr + nyskip)) !!consider nyskip, should be the first year of output
      call stdaa_sqlite_writebasin("END_YEAR",real(iyr - 1))
      call stdaa_sqlite_writebasin("OUTPUT_INTERVAL",real(iprint))
      call stdaa_sqlite_writebasin("SUCCESS",1.0)
      call stdaa_sqlite_writebasin("VERSION",627.0)
      end

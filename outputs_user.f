c outputs_user.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine OUTPUTS_USER(nx,ny,iter,Tair_grid,rh_grid,
     &  uwind_grid,vwind_grid,windspd_grid,winddir_grid,
     &  Qsi_grid,Qli_grid,prec_grid,Tsfc,Qle,Qh,Qe,Qc,Qm,Qf,
     &  e_balance,snow_depth,xro_snow,swe_depth,ro_nsnow,
     &  runoff,rain,sprec,sum_prec,sum_runoff,w_balance,
     &  snow_d,topo_land,wbal_qsubl,sum_sprec,wbal_salt,
     &  wbal_susp,ro_snow_grid,sum_Qcs,canopy_int,Qcs,
     &  iyear,imonth,iday,xhour,undef,deltax,xmn,ymn,
     &  wbal_subgrid,canopy_unload,sum_qsubl,sum_trans,
     &  sum_unload,sum_glacmelt,glacier_melt,swemelt,
     &  iprint_inc,sfc_pressure,sum_swemelt,albedo,
     &  icorr_factor_loop,swesublim,vegtype,iter_start,
     &  iprint_stream)

c This subroutine is available to provide user-defined outputs.
c   These might be special-case situations, like just writing out
c   data at the end of every day, writing out a few grid cells,
c   saving each data arrays to individual files, etc.

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,iter,max_poly,num_poly,iyear,imonth,iday,
     &  iprint_inc,icorr_factor_loop,irun_corr_factor,iter_start
      integer icorr_loop_new

      real Tair_grid(nx_max,ny_max),rh_grid(nx_max,ny_max),
     &  uwind_grid(nx_max,ny_max),vwind_grid(nx_max,ny_max),
     &  windspd_grid(nx_max,ny_max),winddir_grid(nx_max,ny_max),
     &  Qsi_grid(nx_max,ny_max),Qli_grid(nx_max,ny_max),
     &  prec_grid(nx_max,ny_max),Tsfc(nx_max,ny_max),
     &  Qle(nx_max,ny_max),Qh(nx_max,ny_max),Qe(nx_max,ny_max),
     &  Qc(nx_max,ny_max),Qm(nx_max,ny_max),Qf(nx_max,ny_max),
     &  e_balance(nx_max,ny_max),snow_depth(nx_max,ny_max),
     &  xro_snow(nx_max,ny_max),swe_depth(nx_max,ny_max),
     &  ro_nsnow(nx_max,ny_max),runoff(nx_max,ny_max),
     &  rain(nx_max,ny_max),sprec(nx_max,ny_max),
     &  sum_prec(nx_max,ny_max),sum_runoff(nx_max,ny_max),
     &  w_balance(nx_max,ny_max),snow_d(nx_max,ny_max),
     &  topo_land(nx_max,ny_max),wbal_qsubl(nx_max,ny_max),
     &  sum_sprec(nx_max,ny_max),wbal_salt(nx_max,ny_max),
     &  wbal_susp(nx_max,ny_max),ro_snow_grid(nx_max,ny_max),
     &  sum_Qcs(nx_max,ny_max),canopy_int(nx_max,ny_max),
     &  Qcs(nx_max,ny_max),wbal_subgrid(nx_max,ny_max),
     &  canopy_unload(nx_max,ny_max),sum_qsubl(nx_max,ny_max),
     &  sum_trans(nx_max,ny_max),glacier_melt(nx_max,ny_max),
     &  sum_unload(nx_max,ny_max),sum_glacmelt(nx_max,ny_max),
     &  swemelt(nx_max,ny_max),sfc_pressure(nx_max,ny_max),
     &  sum_swemelt(nx_max,ny_max),swesublim(nx_max,ny_max),
     &  vegtype(nx_max,ny_max),albedo(nx_max,ny_max)

c     parameter (max_poly=1002001)

      integer iprint_inc2,naverage

c Averaging arrays start.
      real tair(nx_max,ny_max)
      real rhxx(nx_max,ny_max)
      real wspd(nx_max,ny_max)
      real qsix(nx_max,ny_max)
      real qlix(nx_max,ny_max)
      real qlex(nx_max,ny_max)
      real albd(nx_max,ny_max)
      real qhxx(nx_max,ny_max)
      real qexx(nx_max,ny_max)
	  
      real wdir(nx_max,ny_max)
      real uwnd(nx_max,ny_max)
      real vwnd(nx_max,ny_max)

      real prec(nx_max,ny_max)
      real rpre(nx_max,ny_max)
      real spre(nx_max,ny_max)
      real smlt(nx_max,ny_max)
      real ssub(nx_max,ny_max)
      real roff(nx_max,ny_max)
      real sspr(nx_max,ny_max)
      real ssmt(nx_max,ny_max)

      real snod(nx_max,ny_max)
      real sden(nx_max,ny_max)
      real swed(nx_max,ny_max)
      real gmlt(nx_max,ny_max)
      real csub(nx_max,ny_max)
      real subl(nx_max,ny_max)
	  
      real qmlt(nx_max,ny_max)
      real wbal(nx_max,ny_max)
	  
c Averaging arrays end.

      real undef,xhour,deltax
      double precision xmn,ymn

c     real poly(nx_max,ny_max)
c     real tair_poly(max_poly)
c     real rh_poly(max_poly)
c     real wspd_poly(max_poly)
c     real prec_poly(max_poly)
c     real Qsi_poly(max_poly)
c     real Qli_poly(max_poly)
c     real count_poly(max_poly)
c     real elke
c     character*7 ipoly_num

      character*32 outfname

      character*32 path1
      character*31 path2

      integer individual_files
      real pi,rad2deg
      integer iprint_stream

      pi = 2.0 * acos(0.0)
      rad2deg = 180.0 / pi
 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c individual_files = 1 will write a separate file for each variable.
      individual_files = 1
c      path1 = '/outputs/'
c      path2 = '/outputs/'

c Open individual output files for each variable.
      if (individual_files.eq.1) then

      if (iter.eq.iter_start) then
        if (icorr_factor_loop.eq.1) then
          if (iprint_stream.eq.1) then  
            open (228,file='/datadrive/SnowModel/prec.dat',
     &        STATUS='NEW', access='STREAM')
            open (232,file='/datadrive/SnowModel/ssub.dat',
     &        STATUS='NEW', access='STREAM')
            open (233,file='/datadrive/SnowModel/roff.dat',
     &        STATUS='NEW', access='STREAM')
            open (236,file='/datadrive/SnowModel/swed.dat',
     &        STATUS='NEW', access='STREAM')
            open (241,file='/datadrive/SnowModel/csub.dat',
     &        STATUS='NEW', access='STREAM')
            open (245,file='/datadrive/SnowModel/wbal.dat',
     &        STATUS='NEW', access='STREAM')
          else

c          open (221,file='outputs/wo_assim/tair.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (222,file='outputs/wo_assim/rhxx.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (223,file='outputs/wo_assim/wspd.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (224,file='outputs/wo_assim/qsix.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (225,file='outputs/wo_assim/qlix.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (226,file='outputs/wo_assim/qlex.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (227,file='outputs/wo_assim/albd.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
           open (228, file='outputs/wo_assim/prec.gdat',
     &       form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (229,file='outputs/wo_assim/rpre.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (230,file='outputs/wo_assim/spre.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (231,file='outputs/wo_assim/smlt.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (232,file='outputs/wo_assim/ssub.gdat',
     &       form='unformatted',access='direct',recl=4*1*nx*ny)
          open (233,file='outputs/wo_assim/roff.gdat',
     &       form='unformatted',access='direct',recl=4*1*nx*ny)

c          open (234,file='outputs/wo_assim/snod.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (235,file='outputs/wo_assim/sden.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (236,file='outputs/wo_assim/swed.gdat',
     &       form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (237,file='outputs/wo_assim/sspr.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (238,file='outputs/wo_assim/ssmt.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (239,file='outputs/wo_assim/wdir.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c          open (240,file='outputs/wo_assim/gmlt.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (241,file='outputs/wo_assim/csub.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
cc	      open (243,file='outputs/wo_assim/qhxx.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
c	      open (244,file='outputs/wo_assim/qexx.gdat',
c     &      form='unformatted',access='direct',recl=4*1*nx*ny)
            open (245,file='outputs/wo_assim/wbal.gdat',
     &       form='unformatted',access='direct',recl=4*1*nx*ny)
          endif
        endif

        if (icorr_factor_loop.eq.2) then
          open (321,file=path2//'tair.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (322,file=path2//'rhxx.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (323,file=path2//'wspd.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (324,file=path2//'qsix.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (325,file=path2//'qlix.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (326,file=path2//'qlex.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (327,file=path2//'albd.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)

          open (328,file=path2//'prec.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (329,file=path2//'rpre.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (330,file=path2//'spre.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (331,file=path2//'smlt.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (332,file=path2//'ssub.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (333,file=path2//'roff.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)

          open (334,file=path2//'snod.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (335,file=path2//'sden.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (336,file=path2//'swed.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)

          open (337,file=path2//'sspr.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (338,file=path2//'ssmt.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)

          open (339,file=path2//'wdir.gdat',
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
        endif
      endif

c If you want to save the data every 24-hours, set naverage = 1
c   If you want to save the data at every time step, without the
c   averaging, set naverage = 0
      naverage = 1

c iprint_inc2 = 24 says that you are only going to save the data
c   every 24 time steps.
      iprint_inc2 = 4

      if (naverage.eq.1) then
        if (iter.eq.iter_start) then
c Initialize the averaging and summing arrays.
          do j=1,ny
            do i=1,nx
c Ave.
              tair(i,j) = 0.0
              rhxx(i,j) = 0.0
              wspd(i,j) = 0.0
              qsix(i,j) = 0.0
              qlix(i,j) = 0.0
              qlex(i,j) = 0.0
              albd(i,j) = 0.0
              qmlt(i,j) = 0.0

              uwnd(i,j) = 0.0
              vwnd(i,j) = 0.0
              wdir(i,j) = 0.0
c Sum.
              prec(i,j) = 0.0
              rpre(i,j) = 0.0
              spre(i,j) = 0.0
              smlt(i,j) = 0.0
              ssub(i,j) = 0.0
              roff(i,j) = 0.0
              gmlt(i,j) = 0.0
	          csub(i,j) = 0.0
	          subl(i,j) = 0.0
              wbal(i,j) = 0.0
c End of day.
              snod(i,j) = 0.0
              sden(i,j) = 0.0
              swed(i,j) = 0.0
              sspr(i,j) = 0.0
              ssmt(i,j) = 0.0
            enddo
          enddo
        endif

c Average.
        do j=1,ny
          do i=1,nx
c Ave.
            tair(i,j) = tair(i,j) + (Tair_grid(i,j) - 273.16) /
     &        real(iprint_inc2)
            rhxx(i,j) = rhxx(i,j) + rh_grid(i,j) / real(iprint_inc2)
            wspd(i,j) = wspd(i,j) + windspd_grid(i,j) /
     &        real(iprint_inc2)
            qsix(i,j) = qsix(i,j) + Qsi_grid(i,j) / real(iprint_inc2)
            qlix(i,j) = qlix(i,j) + Qli_grid(i,j) / real(iprint_inc2)
            qlex(i,j) = qlex(i,j) + Qle(i,j) / real(iprint_inc2)
            albd(i,j) = albd(i,j) + albedo(i,j) / real(iprint_inc2)
            qmlt(i,j) = qmlt(i,j) + Qm(i,j) / real(iprint_inc2)

            uwnd(i,j) = uwnd(i,j) + uwind_grid(i,j) / real(iprint_inc2)
            vwnd(i,j) = vwnd(i,j) + vwind_grid(i,j) / real(iprint_inc2)

c Some compilers do not allow both u and v to be 0.0 in
c   the atan2 computation.
            if (abs(uwnd(i,j)).lt.1e-10) uwnd(i,j) = 1e-10

            wdir(i,j) = rad2deg * atan2(uwnd(i,j),vwnd(i,j))
            if (wdir(i,j).ge.180.0) then
              wdir(i,j) = wdir(i,j) - 180.0
            else
              wdir(i,j) = wdir(i,j) + 180.0
            endif

c Sum.
            prec(i,j) = prec(i,j) + prec_grid(i,j)
            rpre(i,j) = rpre(i,j) + rain(i,j)
            spre(i,j) = spre(i,j) + sprec(i,j)
            smlt(i,j) = smlt(i,j) + swemelt(i,j)
            ssub(i,j) = ssub(i,j) + swesublim(i,j)
            roff(i,j) = roff(i,j) + runoff(i,j)
            gmlt(i,j) = gmlt(i,j) + glacier_melt(i,j)
	        csub(i,j) = csub(i,j) + Qcs(i,j)
		    subl(i,j) = subl(i,j) + wbal_qsubl(i,j)
		    wbal(i,j) = wbal(i,j) + w_balance(i,j)			
c End of day.
            snod(i,j) = snow_depth(i,j)
            sden(i,j) = xro_snow(i,j)
            swed(i,j) = swe_depth(i,j)
            sspr(i,j) = sum_sprec(i,j)
            ssmt(i,j) = sum_swemelt(i,j)
          enddo
        enddo

c Print out the data at the end of each day.
        if (mod(iter,iprint_inc2).eq.0.0) then

c Mask out the ocean points (vegtype=24).  Note that this is only
c   working if you are averaging the data (if you are printing the
c   data at every time step, this is not done).
         do j=1,ny
           do i=1,nx
             if (vegtype(i,j).eq.24.0) then
c Ave.
               tair(i,j) = undef
               rhxx(i,j) = undef
               wspd(i,j) = undef
               qsix(i,j) = undef
               qlix(i,j) = undef
               qlex(i,j) = undef
               albd(i,j) = undef
               wdir(i,j) = undef
			   
			   qmlt(i,j) = undef
c Sum.
               prec(i,j) = undef
               rpre(i,j) = undef
               spre(i,j) = undef
               smlt(i,j) = undef
               ssub(i,j) = undef
               roff(i,j) = undef
			   gmlt(i,j) = undef
			   csub(i,j) = undef
			   subl(i,j) = undef
			   wbal(i,j) = undef
c End of day.
               snod(i,j) = undef
               sden(i,j) = undef
               swed(i,j) = undef
               sspr(i,j) = undef
               ssmt(i,j) = undef
             endif
           enddo
         enddo

          if (icorr_factor_loop.eq.1) then

c            write (221,rec=iter/iprint_inc2) ((tair(i,j),i=1,nx),j=1,ny)
c            write (222,rec=iter/iprint_inc2) ((rhxx(i,j),i=1,nx),j=1,ny)
c            write (223,rec=iter/iprint_inc2) ((wspd(i,j),i=1,nx),j=1,ny)
c            write (224,rec=iter/iprint_inc2) ((qsix(i,j),i=1,nx),j=1,ny)
c            write (225,rec=iter/iprint_inc2) ((qlix(i,j),i=1,nx),j=1,ny)
c            write (226,rec=iter/iprint_inc2) ((qlex(i,j),i=1,nx),j=1,ny)
c            write (227,rec=iter/iprint_inc2) ((albd(i,j),i=1,nx),j=1,ny)

            write (228) ((prec(i,j),i=1,nx),j=1,ny)
c            write (229,rec=iter/iprint_inc2) ((rpre(i,j),i=1,nx),j=1,ny)
c            write (230,rec=iter/iprint_inc2) ((spre(i,j),i=1,nx),j=1,ny)
c            write (231,rec=iter/iprint_inc2) ((smlt(i,j),i=1,nx),j=1,ny)
            write (232) ((ssub(i,j),i=1,nx),j=1,ny)
            write (233) ((roff(i,j),i=1,nx),j=1,ny)

c            write (234) ((snod(i,j),i=1,nx),j=1,ny)
c            write (235) ((sden(i,j),i=1,nx),j=1,ny)
            write (236) ((swed(i,j),i=1,nx),j=1,ny)

c            write (237,rec=iter/iprint_inc2) ((sspr(i,j),i=1,nx),j=1,ny)
c            write (238,rec=iter/iprint_inc2) ((ssmt(i,j),i=1,nx),j=1,ny)

c            write (239,rec=iter/iprint_inc2) ((wdir(i,j),i=1,nx),j=1,ny)

c            write (240,rec=iter/iprint_inc2) ((gmlt(i,j),i=1,nx),j=1,ny)
            write (241) ((csub(i,j),i=1,nx),j=1,ny)
            write (245) ((wbal(i,j),i=1,nx),j=1,ny)

          elseif (icorr_factor_loop.eq.2) then

            write (321,rec=iter/iprint_inc2) ((tair(i,j),i=1,nx),j=1,ny)
            write (322,rec=iter/iprint_inc2) ((rhxx(i,j),i=1,nx),j=1,ny)
            write (323,rec=iter/iprint_inc2) ((wspd(i,j),i=1,nx),j=1,ny)
            write (324,rec=iter/iprint_inc2) ((qsix(i,j),i=1,nx),j=1,ny)
            write (325,rec=iter/iprint_inc2) ((qlix(i,j),i=1,nx),j=1,ny)
            write (326,rec=iter/iprint_inc2) ((qlex(i,j),i=1,nx),j=1,ny)
            write (327,rec=iter/iprint_inc2) ((albd(i,j),i=1,nx),j=1,ny)

            write (328,rec=iter/iprint_inc2) ((prec(i,j),i=1,nx),j=1,ny)
            write (329,rec=iter/iprint_inc2) ((rpre(i,j),i=1,nx),j=1,ny)
            write (330,rec=iter/iprint_inc2) ((spre(i,j),i=1,nx),j=1,ny)
            write (331,rec=iter/iprint_inc2) ((smlt(i,j),i=1,nx),j=1,ny)
            write (332,rec=iter/iprint_inc2) ((ssub(i,j),i=1,nx),j=1,ny)
            write (333,rec=iter/iprint_inc2) ((roff(i,j),i=1,nx),j=1,ny)

            write (334,rec=iter/iprint_inc2) ((snod(i,j),i=1,nx),j=1,ny)
            write (335,rec=iter/iprint_inc2) ((sden(i,j),i=1,nx),j=1,ny)
            write (336,rec=iter/iprint_inc2) ((swed(i,j),i=1,nx),j=1,ny)

            write (337,rec=iter/iprint_inc2) ((sspr(i,j),i=1,nx),j=1,ny)
            write (338,rec=iter/iprint_inc2) ((ssmt(i,j),i=1,nx),j=1,ny)

            write (339,rec=iter/iprint_inc2) ((wdir(i,j),i=1,nx),j=1,ny)

          endif

c Reinitialize the summing arrays.
          do j=1,ny
            do i=1,nx
c Ave.
              tair(i,j) = 0.0
              rhxx(i,j) = 0.0
              wspd(i,j) = 0.0
              qsix(i,j) = 0.0
              qlix(i,j) = 0.0
              qlex(i,j) = 0.0
              albd(i,j) = 0.0

              uwnd(i,j) = 0.0
              vwnd(i,j) = 0.0
              wdir(i,j) = 0.0
c Sum.
              prec(i,j) = 0.0
              rpre(i,j) = 0.0
              spre(i,j) = 0.0
              smlt(i,j) = 0.0
              ssub(i,j) = 0.0
              roff(i,j) = 0.0
              gmlt(i,j) = 0.0
              csub(i,j) = 0.0
c End of day.
              snod(i,j) = 0.0
              sden(i,j) = 0.0
              swed(i,j) = 0.0
              sspr(i,j) = 0.0
              ssmt(i,j) = 0.0
            enddo
          enddo
        endif
      endif

c Save the data at every time step, without the averaging, etc.
      if (naverage.eq.0) then

c Save the data at every time step.
        if (icorr_factor_loop.eq.1) then

c          write (221,rec=iter) ((Tair_grid(i,j)-273.16,i=1,nx),j=1,ny)
c          write (222,rec=iter) ((rh_grid(i,j),i=1,nx),j=1,ny)
c          write (223,rec=iter) ((windspd_grid(i,j),i=1,nx),j=1,ny)
c          write (224,rec=iter) ((Qsi_grid(i,j),i=1,nx),j=1,ny)
c          write (225,rec=iter) ((Qli_grid(i,j),i=1,nx),j=1,ny)
c          write (226,rec=iter) ((Qle(i,j),i=1,nx),j=1,ny)
c          write (227,rec=iter) ((albedo(i,j),i=1,nx),j=1,ny)

c          write (228,rec=iter) ((prec_grid(i,j),i=1,nx),j=1,ny)
c          write (229,rec=iter) ((rain(i,j),i=1,nx),j=1,ny)
c          write (230,rec=iter) ((sprec(i,j),i=1,nx),j=1,ny)
c          write (231,rec=iter) ((swemelt(i,j),i=1,nx),j=1,ny)
c          write (232,rec=iter) ((swesublim(i,j),i=1,nx),j=1,ny)
c          write (233,rec=iter) ((runoff(i,j),i=1,nx),j=1,ny)

c          write (234,rec=iter) ((snow_depth(i,j),i=1,nx),j=1,ny)
c          write (235,rec=iter) ((xro_snow(i,j),i=1,nx),j=1,ny)
c          write (236,rec=iter) ((swe_depth(i,j),i=1,nx),j=1,ny)

c          write (237,rec=iter) ((sum_sprec(i,j),i=1,nx),j=1,ny)
c          write (238,rec=iter) ((sum_swemelt(i,j),i=1,nx),j=1,ny)

c          write (239,rec=iter) ((winddir_grid(i,j),i=1,nx),j=1,ny)
c          write (240,rec=iter) ((glacier_melt(i,j),i=1,nx),j=1,ny)
c          write (241,rec=iter) ((Qcs(i,j),i=1,nx),j=1,ny)
c          write (242,rec=iter) ((Qm(i,j),i=1,nx),j=1,ny)
c          write (243,rec=iter) ((Qh(i,j),i=1,nx),j=1,ny)
c          write (244,rec=iter) ((Qe(i,j),i=1,nx),j=1,ny)

        elseif (icorr_factor_loop.eq.2) then

          write (321,rec=iter) ((Tair_grid(i,j)-273.16,i=1,nx),j=1,ny)
          write (322,rec=iter) ((rh_grid(i,j),i=1,nx),j=1,ny)
          write (323,rec=iter) ((windspd_grid(i,j),i=1,nx),j=1,ny)
          write (324,rec=iter) ((Qsi_grid(i,j),i=1,nx),j=1,ny)
          write (325,rec=iter) ((Qli_grid(i,j),i=1,nx),j=1,ny)
          write (326,rec=iter) ((Qle(i,j),i=1,nx),j=1,ny)
          write (327,rec=iter) ((albedo(i,j),i=1,nx),j=1,ny)

          write (328,rec=iter) ((prec_grid(i,j),i=1,nx),j=1,ny)
          write (329,rec=iter) ((rain(i,j),i=1,nx),j=1,ny)
          write (330,rec=iter) ((sprec(i,j),i=1,nx),j=1,ny)
          write (331,rec=iter) ((swemelt(i,j),i=1,nx),j=1,ny)
          write (332,rec=iter) ((swesublim(i,j),i=1,nx),j=1,ny)
          write (333,rec=iter) ((runoff(i,j),i=1,nx),j=1,ny)

          write (334,rec=iter) ((snow_depth(i,j),i=1,nx),j=1,ny)
          write (335,rec=iter) ((xro_snow(i,j),i=1,nx),j=1,ny)
          write (336,rec=iter) ((swe_depth(i,j),i=1,nx),j=1,ny)

          write (337,rec=iter) ((sum_sprec(i,j),i=1,nx),j=1,ny)
          write (338,rec=iter) ((sum_swemelt(i,j),i=1,nx),j=1,ny)

          write (339,rec=iter) ((winddir_grid(i,j),i=1,nx),j=1,ny)

        endif

      endif

      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c EXAMPLE OF SAVING DATA IN ASCII/TEXT FORMAT.

c Save the Tair_grid data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_1(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    Tair_grid,deltax,xmn,ymn)
c      endif
      
c Save the rh_grid data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_2(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    rh_grid,deltax,xmn,ymn)
c      endif
      
c Save the windspd_grid data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_3(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    windspd_grid,deltax,xmn,ymn)
c      endif
      
c Save the prec_grid data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_4(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    prec_grid,deltax,xmn,ymn)
c      endif
      
c Save the Qsi_grid data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_5(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    Qsi_grid,deltax,xmn,ymn)
c      endif
      
c Save the Qli_grid data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_6(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    Qli_grid,deltax,xmn,ymn)
c      endif
      
c Save the swe_depth data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_7(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    swe_depth,deltax,xmn,ymn)
c      endif
      
c Save the runoff data at the end of every day.
c      if (mod(iter,iprint_inc).eq.0.0) then
c        call ascii_outputs_8(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    runoff,deltax,xmn,ymn)
c      endif
      

cc Save the Tair data every hour for the first day of the simulation.
c      if (iter.le.24) then
c        call ascii_outputs_2(nx,ny,iyear,imonth,iday,xhour,undef,
c     &    Tair_grid,deltax,xmn,ymn)
c      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c EXAMPLE OF SAVING DATA IN GRADS/BINARY FORMAT.

c Save the data at the end of every day, when the model is running
c   at a sub-daily time step.

c       if (mod(iter,iprint_inc2).eq.0.0) then
c         open (21,file='outputs/prec.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)
c         open (22,file='outputs/smlt.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)
c         open (23,file='outputs/ssub.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)
c         open (24,file='outputs/roff.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)
c         open (25,file='outputs/swed.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)
c         open (26,file='outputs/sspr.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)
c         open (27,file='outputs/ssmt.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)  
c         open (28,file='outputs/spre.gdat',
c     &        form='unformatted',access='direct',recl=4*1*nx*ny)   

c        write (21,rec=iter/iprint_inc2) ((prec_grid(i,j),i=1,nx),j=1,ny)
c        write (22,rec=iter/iprint_inc2) ((swemelt(i,j),i=1,nx),j=1,ny)
c        write (23,rec=iter/iprint_inc2) ((swesublim(i,j),i=1,nx),j=1,ny)
c        write (24,rec=iter/iprint_inc2) ((runoff(i,j),i=1,nx),j=1,ny)
c        write (25,rec=iter/iprint_inc2) ((swe_depth(i,j),i=1,nx),j=1,ny)
c        write (26,rec=iter/iprint_inc2) ((sum_sprec(i,j),i=1,nx),j=1,ny)
c        write (27,rec=iter/iprint_inc2)
c     &         ((sum_swemelt(i,j),i=1,nx),j=1,ny)
c        write (28,rec=iter/iprint_inc2) ((sum_prec(i,j),i=1,nx),j=1,ny)
c       endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c EXAMPLE OF SAVING DATA IN GRADS/BINARY FORMAT.

c Save the data at every iteration, with each variable saved to a
c   different file (good for large simulation domains).

c       if (iter.eq.1) then
c       open (21,file='outputs/tair.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)
c       open (22,file='outputs/rh.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)
c       open (23,file='outputs/wspd.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)
c       open (24,file='outputs/prec.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)
c       open (25,file='outputs/qsi.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)
c       open (26,file='outputs/qli.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)
c       open (27,file='outputs/swed.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)  
c       open (21,file='outputs/roff.gdat',
c     &    form='unformatted',access='direct',recl=4*1*nx*ny)   
c       endif
c      write (21,rec=iter) ((Tair_grid(i,j),i=1,nx),j=1,ny)
c      write (22,rec=iter) ((rh_grid(i,j),i=1,nx),j=1,ny)
c      write (23,rec=iter) ((windspd_grid(i,j),i=1,nx),j=1,ny)
c      write (24,rec=iter) ((prec_grid(i,j),i=1,nx),j=1,ny)
c      write (25,rec=iter) ((Qsi_grid(i,j),i=1,nx),j=1,ny)
c      write (26,rec=iter) ((Qli_grid(i,j),i=1,nx),j=1,ny)
c      write (27,rec=iter) ((swe_depth(i,j),i=1,nx),j=1,ny)
c      write (21,rec=iter) ((runoff(i,j),i=1,nx),j=1,ny)
     

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     elke = 0.0
c     if (elke.eq.1.0) then
c Perform the preprocessing for the polygon outputs to be used by
c   FASST and SNTHERM.
c       if (iter.eq.1) then
c         open (47,file='polys/fraser.polys.200m.gdat',
c    &      form='unformatted',access='direct',recl=4*nx*ny)

c         read (47,rec=1) ((poly(i,j),i=1,nx),j=1,ny)

c Generate a table describing the number of grid cells in each
c   polygon.
c         do num_poly=1,max_poly
c           count_poly(num_poly) = 0.0
c         enddo
c         do j=1,ny
c           do i=1,nx
c             num_poly = nint(poly(i,j))
c             if (num_poly.ne.undef)
c    &        count_poly(num_poly) = count_poly(num_poly) + 1.0
c           enddo
c         enddo

c Open the polygon output files.
c         do num_poly=1,max_poly
c           if (count_poly(num_poly).gt.0.0) then
c             write(ipoly_num,'(i7.7)') num_poly
c             outfname = 'polys/fraser.polygon-'//ipoly_num//'.dat'
c             open (num_poly+100,file=outfname,form='formatted')
c           endif
c         enddo
c       endif

c Initialize the polygon averaging array.
c       do num_poly=1,max_poly
c         tair_poly(num_poly) = 0.0
c         rh_poly(num_poly) = 0.0
c         wspd_poly(num_poly) = 0.0
c         prec_poly(num_poly) = 0.0
c         Qsi_poly(num_poly) = 0.0
c         Qli_poly(num_poly) = 0.0
c       enddo

c  Calculate the polygon averages be used by FASST and SNTHERM.
c       call ave_poly_var(nx,ny,poly,count_poly,tair_poly,Tair_grid,
c    &    undef,max_poly)

c       call ave_poly_var(nx,ny,poly,count_poly,rh_poly,rh_grid,
c    &    undef,max_poly)

c       call ave_poly_var(nx,ny,poly,count_poly,wspd_poly,windspd_grid,
c    &    undef,max_poly)

c       call ave_poly_var(nx,ny,poly,count_poly,prec_poly,prec_grid,
c    &    undef,max_poly)

c       call ave_poly_var(nx,ny,poly,count_poly,Qsi_poly,Qsi_grid,
c    &    undef,max_poly)

c       call ave_poly_var(nx,ny,poly,count_poly,Qli_poly,Qli_grid,
c    &    undef,max_poly)

c Write the data to individual polygon files.
c       do num_poly=1,max_poly
c         if (count_poly(num_poly).gt.0.0)
c    &      write (num_poly+100,88) iyear,imonth,iday,xhour,
c    &        num_poly,tair_poly(num_poly)-273.16,rh_poly(num_poly),
c    &        wspd_poly(num_poly),prec_poly(num_poly),
c    &        Qsi_poly(num_poly),Qli_poly(num_poly)
c       enddo

c 88    format (i5,i3,i3,f6.2,i8,3f10.3,f12.6,2f10.3)
c     endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_1(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  Tair_grid,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real Tair_grid(nx_max,ny_max)

c      character*18 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/Tair_grid_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (23,file=fname)

c      write (23,*) 'ncols        ',nx
c      write (23,*) 'nrows        ',ny
c      write (23,*) 'xllcorner    ',xmn
c      write (23,*) 'yllcorner    ',ymn
c      write (23,*) 'cellsize     ',deltax
c      write (23,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (23,form) (Tair_grid(i,j),i=1,nx)
c      enddo

c      close (23)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_2(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  rh_grid,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real rh_grid(nx_max,ny_max)

c      character*16 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/rh_grid_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (24,file=fname)

c      write (24,*) 'ncols        ',nx
c      write (24,*) 'nrows        ',ny
c      write (24,*) 'xllcorner    ',xmn
c      write (24,*) 'yllcorner    ',ymn
c      write (24,*) 'cellsize     ',deltax
c      write (24,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (24,form) (rh_grid(i,j),i=1,nx)
c      enddo

c      close (24)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_3(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  windspd_grid,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real windspd_grid(nx_max,ny_max)

c      character*21 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/windspd_grid_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (25,file=fname)

c      write (25,*) 'ncols        ',nx
c      write (25,*) 'nrows        ',ny
c      write (25,*) 'xllcorner    ',xmn
c      write (25,*) 'yllcorner    ',ymn
c      write (25,*) 'cellsize     ',deltax
c      write (25,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (25,form) (windspd_grid(i,j),i=1,nx)
c      enddo

c      close (25)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_4(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  prec_grid,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real prec_grid(nx_max,ny_max)

c      character*18 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/prec_grid_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (26,file=fname)

c      write (26,*) 'ncols        ',nx
c      write (26,*) 'nrows        ',ny
c      write (26,*) 'xllcorner    ',xmn
c      write (26,*) 'yllcorner    ',ymn
c      write (26,*) 'cellsize     ',deltax
c      write (26,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (26,form) (prec_grid(i,j),i=1,nx)
c      enddo

c      close (26)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_5(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  Qsi_grid,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real Qsi_grid(nx_max,ny_max)

c      character*17 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/Qsi_grid_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (27,file=fname)

c      write (27,*) 'ncols        ',nx
c      write (27,*) 'nrows        ',ny
c      write (27,*) 'xllcorner    ',xmn
c      write (27,*) 'yllcorner    ',ymn
c      write (27,*) 'cellsize     ',deltax
c      write (27,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (27,form) (Qsi_grid(i,j),i=1,nx)
c      enddo

c      close (27)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_6(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  Qli_grid,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real Qli_grid(nx_max,ny_max)

c      character*17 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/Qli_grid_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (28,file=fname)

c      write (28,*) 'ncols        ',nx
c      write (28,*) 'nrows        ',ny
c      write (28,*) 'xllcorner    ',xmn
c      write (28,*) 'yllcorner    ',ymn
c      write (28,*) 'cellsize     ',deltax
c      write (28,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (28,form) (Qli_grid(i,j),i=1,nx)
c      enddo

c      close (28)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_7(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  swe_depth,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real swe_depth(nx_max,ny_max)

c      character*18 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/swe_depth_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (29,file=fname)

c      write (29,*) 'ncols        ',nx
c      write (29,*) 'nrows        ',ny
c      write (29,*) 'xllcorner    ',xmn
c      write (29,*) 'yllcorner    ',ymn
c      write (29,*) 'cellsize     ',deltax
c      write (29,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (29,form) (swe_depth(i,j),i=1,nx)
c      enddo

c      close (29)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine ascii_outputs_8(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  runoff,deltax,xmn,ymn)

c      implicit none

c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real runoff(nx_max,ny_max)

c      character*15 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*35 fname
c      character*40 form

c      name1 = 'outputs/runoff_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (30,file=fname)

c      write (30,*) 'ncols        ',nx
c      write (30,*) 'nrows        ',ny
c      write (30,*) 'xllcorner    ',xmn
c      write (30,*) 'yllcorner    ',ymn
c      write (30,*) 'cellsize     ',deltax
c      write (30,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (30,form) (runoff(i,j),i=1,nx)
c      enddo

c      close (30)

c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      subroutine ascii_outputs_2(nx,ny,iyear,imonth,iday,xhour,undef,
c     &  Tair_grid,deltax,xmn,ymn)
c
c      implicit none
c
c      include 'snowmodel.inc'

c      integer i,j,nx,ny,iyear,imonth,iday,ihour
c      real undef,xhour,deltax
c      double precision xmn,ymn
c      real Tair_grid(nx_max,ny_max)
c
c      character*13 name1
c      character*1 dot
c      character*4 name2
c      character*4 yyyy
c      character*2 mm
c      character*2 dd
c      character*2 hh
c      character*30 fname
c      character*40 form

c      name1 = 'outputs/Tair_'
c      name2 = '.asc'
c      dot = '.'

c      ihour = nint(xhour)
c      write(yyyy,'(i4.4)') iyear
c      write(mm,'(i2.2)') imonth
c      write(dd,'(i2.2)') iday
c      write(hh,'(i2.2)') ihour
c      fname = name1//yyyy//dot//mm//dot//dd//dot//hh//name2

c      open (23,file=fname)

c      write (23,*) 'ncols        ',nx
c      write (23,*) 'nrows        ',ny
c      write (23,*) 'xllcorner    ',xmn
c      write (23,*) 'yllcorner    ',ymn
c      write (23,*) 'cellsize     ',deltax
c      write (23,*) 'NODATA_value ',undef

c Define the output format.  The following will produce nx columns
c   in the ascii output array (like assumed in an ARC/INFO GRID
c   ascii data file.  nx is getting written to the i5 space.  The
c   output format for the data values will be f12.4 (this is what
c   you might want to modify, depending on the data units/values).
c      write (form,90) nx
c  90  format ('(',i5,'f12.4)')

c      do j=ny,1,-1
c        write (23,form) (Tair_grid(i,j)-273.16,i=1,nx)
c      enddo
c
c      close (23)
c
c      return
c      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     subroutine ave_poly_var(nx,ny,poly,count_poly,ave_poly,var,
c    &  undef,max_poly)

c     implicit none

c     include 'snowmodel.inc'

c     integer i,j,nx,ny,num_poly,max_poly
c     real undef
c     real poly(nx_max,ny_max)
c     real var(nx_max,ny_max)
c     real ave_poly(max_poly)

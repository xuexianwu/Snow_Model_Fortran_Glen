c dataassim_user.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine DATAASSIM_USER(nx,ny,icorr_factor_index,
     &  corr_factor,max_iter,snowpack_output_fname,deltax,
     &  deltay,xmn,ymn,dt,nobs_dates)

c Perform the required correction (precipitation and melt) factor
c   calculations.

c This program works when you have data at one or many points
c   (many individual grid cells), for one or more times.  And for
c   data (e.g., average swe) over areas of grid cells; there can be
c   many of these areas, at many different times.

ccccccccccccccccc
cccccccc
c BEGIN USER EDIT SECTION.

c The notation followed in this subroutine is that user-modified
c   sections are enclosed within these kinds of 'ccccc' lines (see
c   just above and below this text).

c END USER EDIT SECTION.
cccccccc
ccccccccccccccccc

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,undef,dn,beta,areas_flag,swe_count,dt,
     &  sprec_ratio(max_obs_dates),smelt_ratio(max_obs_dates)
      double precision xmn,ymn
      real corr_factor(nx_max,ny_max,max_obs_dates+1)
      real corr_factor_tmp(nx_max*ny_max),cf_min
      real swe_tmp(nx_max,ny_max),sum_sprec_tmp1(nx_max,ny_max),
     &  sum_sprec_tmp2(nx_max,ny_max),grid(nx_max,ny_max),
     &  areas_mask(nx_max,ny_max),sum_smelt_tmp1(nx_max,ny_max),
     &  sum_smelt_tmp2(nx_max,ny_max)
      integer icorr_factor_index(max_time_steps)
      double precision xstn(nx_max*ny_max),ystn(nx_max*ny_max)
      real swe_obs(nx_max*ny_max),swe_model(nx_max*ny_max),
     &  sumsprec_model(nx_max*ny_max),delta_old(nx_max*ny_max),
     &  corr_offset(nx_max*ny_max),obsid(nx_max*ny_max),
     &  sumsmelt_model(nx_max*ny_max)

      real obsid_old(nx_max*ny_max),delta_old_tmp(nx_max*ny_max)
      integer nstns_old,kk

      integer iobs_rec(max_obs_dates)
      integer ii(nx_max*ny_max),jj(nx_max*ny_max)
      integer iobs_num,irec1,irec2,nobs_dates,nx,ny,i,j,nvars,
     &  ifill,iobsint,k,nstns,max_iter

      character*80 snowpack_output_fname

      cf_min = 0.1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc

ccccccccccccccccc
cccccccc
c BEGIN USER EDIT SECTION.

c Open a file to write some basic correction factor information.
      open (unit=77,file='data/corr_factor.txt')

c Open an output file for the correction factor array.
      open(62,file='data/corr_factor.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Define whether this simulation will be processing areas (data
c   within groups of grid cells: areas_flag = 1.0), or points
c   (single grid cells: areas_flag = 0.0).  Note that if you have
c   a combination of areas and points, you have to use the areas
c   option and treat each point like a single-grid-cell (small)
c   area.
c     areas_flag = 0.0
      areas_flag = 1.0

c  Beta controls the interpolation distance weights.  Beta = 1.0
c    will give you a very smooth field, and correction factor
c    distributions that may not produce swe's that exactly match
c    the observations.  Beta << 1.0 will give you correction factor
c    fields that go right through the data.  If you just have one
c    data point/area, beta is not used.
      beta = 1.0
c     beta = 0.1
c     beta = 0.5

c Define the number of observation dates and the associated record 
c   in the grads output file.
      nobs_dates = 1
c     nobs_dates = 2
c Note that this is for hourly outputs, if you use daily outputs
c   this record will be more like 200 (both correspond to
c   30 April 2009).

c 234 = 21 April 2008, for a 1 Sep run start.
      iobs_rec(1) = 234
c     iobs_rec(1) = 5616

c 228 = 16 April 2009, for a 1 Sep run start.
c     iobs_rec(1) = 228
c     iobs_rec(1) = 5472

c If this is an areas simulation, open and read in the areas mask
c   data.  Note that here I assume that the area mask is a nx by ny
c   file with undef values everywhere except at the area 'stations'.
c   And that each 'station' area is given a 1.0, 2.0, etc. that
c   corresponds to the order of the station listing in the 'station'
c   data input file (the first 'station' listed has mask value = 1.0,
c   the second listed has mask value = 2.0, etc.
      if (areas_flag.eq.1.0) then
c       open(63,file='sweobs/nea.obsmask.500m.2008.gdat',
        open(63,file='sweobs/nea.obsmask.100m.2008.gdat',
c       open(63,file='sweobs/nea.obsmask.100m.2009.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read(63,rec=1) ((areas_mask(i,j),i=1,nx),j=1,ny)

c If you have two masks for two different observation dates.
c       open(63,file='data/zack_obs_mask.gdat',
c    &    form='unformatted',access='direct',recl=4*nx*ny)
c       read(63,rec=1) ((areas_mask(i,j,1),i=1,nx),j=1,ny)
c       read(63,rec=2) ((areas_mask(i,j,2),i=1,nx),j=1,ny)
      endif

c Define the number of varibles in the SnowModel output file
c   (e.g., snowpack_output_fname) that will be used to get the
c    model-simulated swe data.
c     nvars = 16
      nvars = 1

c Loop through the observation dates.
      do iobs_num=1,nobs_dates

c For each observation date, read in the data describing the
c   location and swe values for each observation.  For areas
c   simulations, xstn, and ystn correspond to the center of the
c   area domain (these are not really used).
        if (iobs_num.eq.1) then
          open (unit=61,file='data/nea_2008_sweobs.dat')
c         open (unit=61,file='data/nea_2009_sweobs.dat')
          read(61,*) nstns
          do k=1,nstns
            read(61,*) obsid(k),xstn(k),ystn(k),swe_obs(k)
          enddo
          close (61)
c       elseif (iobs_num.eq.2) then
c         open (unit=61,file='data/swe.obs.227.dat')
c         read(61,*) nstns
c         do k=1,nstns
c           read(61,*) obsid(k),xstn(k),ystn(k),swe_obs(k)
c         enddo
c         close (61)
c       elseif (iobs_num.eq.3) then
c         open (unit=61,file='data/swe.obs.263.dat')
c         read(61,*) nstns
c         do k=1,nstns
c           read(61,*) obsid(k),xstn(k),ystn(k),swe_obs(k)
c         enddo
c         close (61)
        endif

c END USER EDIT SECTION.
cccccccc
ccccccccccccccccc

c Convert the x and y locations to (ii,jj) locations.
        do k=1,nstns
          ii(k) = 1 + nint((xstn(k) - xmn) / deltax)
          jj(k) = 1 + nint((ystn(k) - ymn) / deltay)
        enddo

ccccccccccccccccc
cccccccc
c BEGIN USER EDIT SECTION.

c Open up the model output file and extract the model-simulated swe
c   at the observation point and observation time of interest.  Note
c   that in my grads output file, irec = (iter - 1) * nvars, for
c   the case where data are written out at each model time step.

c The reason the output file is closed, opened, and reclosed, is
c   because I want to open it with a different record length than
c   used in the standard SnowModel output files.
c       close (83)
c       open (83,file=snowpack_output_fname,
c    &    form='unformatted',access='direct',recl=4*nx*ny)

c Open swe depth (swe_depth).
c   /outputs/wo_assim/swed.gdat is unit 236 in outputs_user.f

c Open sum snow precip (sum_sprec).
c   /outputs/wo_assim/sspr.gdat is unit 237 in outputs_user.f

c Open sum snow melt (sum_smelt).
c   /outputs/wo_assim/ssmt.gdat is unit 238 in outputs_user.f

c If you do a data assimilation run from start to finish, it is
c   not required to close and reopen these files.  But if you are
c   doing a history restart then these files are no longer open
c   so you must do this.
      close (236)
      close (237)
      close (238)

      open (236,file='/data10/norway/outputs/wo_assim/swed.gdat',
     &  form='unformatted',access='direct',recl=4*1*nx*ny)
      open (237,file='/data10/norway/outputs/wo_assim/sspr.gdat',
     &  form='unformatted',access='direct',recl=4*1*nx*ny)
      open (238,file='/data10/norway/outputs/wo_assim/ssmt.gdat',
     &  form='unformatted',access='direct',recl=4*1*nx*ny)

c Read the model output for the first observation time.
        if (iobs_num.eq.1) then
c         irec1 = (iobs_rec(iobs_num) - 1) * nvars

          irec1 = iobs_rec(iobs_num)

c         read(83,rec=irec1+3) ((swe_tmp(i,j),i=1,nx),j=1,ny)
c         read(83,rec=irec1+11) ((sum_sprec_tmp1(i,j),i=1,nx),j=1,ny)
c         read(83,rec=irec1+14) ((sum_smelt_tmp1(i,j),i=1,nx),j=1,ny)

          read(236,rec=irec1) ((swe_tmp(i,j),i=1,nx),j=1,ny)
          read(237,rec=irec1) ((sum_sprec_tmp1(i,j),i=1,nx),j=1,ny)
          read(238,rec=irec1) ((sum_smelt_tmp1(i,j),i=1,nx),j=1,ny)

c For points, just pull the data at the appropriate grid cell.
c   For areas, average the data over the masked out area for each
c   'station'.
          do k=1,nstns
            if (areas_flag.eq.0.0) then
              swe_model(k) = swe_tmp(ii(k),jj(k))
              sumsprec_model(k) = sum_sprec_tmp1(ii(k),jj(k))
              sumsmelt_model(k) = sum_smelt_tmp1(ii(k),jj(k))
            elseif (areas_flag.eq.1.0) then
              swe_model(k) = 0.0
              sumsprec_model(k) = 0.0
              sumsmelt_model(k) = 0.0
              swe_count = 0.0
              do j=1,ny
                do i=1,nx
                  if (areas_mask(i,j).eq.obsid(k)) then
c                 if (areas_mask(i,j,iobs_num).eq.obsid(k)) then
                    swe_count = swe_count + 1.0
                    swe_model(k) = swe_model(k) + swe_tmp(i,j)
                    sumsprec_model(k) = sumsprec_model(k) +
     &                sum_sprec_tmp1(i,j)
                    sumsmelt_model(k) = sumsmelt_model(k) +
     &                sum_smelt_tmp1(i,j)
                  endif
                enddo
              enddo
              swe_model(k) = swe_model(k) / swe_count
              sumsprec_model(k) = sumsprec_model(k) / swe_count
              sumsmelt_model(k) = sumsmelt_model(k) / swe_count
            endif
          enddo
        endif

c Read the model output for any additional observation times (irec1
c   = current obs time, irec2 = previous obs time).
        if (iobs_num.gt.1) then
c         irec1 = (iobs_rec(iobs_num) - 1) * nvars
c         irec2 = (iobs_rec(iobs_num-1) - 1) * nvars

          irec1 = iobs_rec(iobs_num)
          irec2 = iobs_rec(iobs_num-1)

c         read(83,rec=irec1+3) ((swe_tmp(i,j),i=1,nx),j=1,ny)
c         read(83,rec=irec1+11) ((sum_sprec_tmp1(i,j),i=1,nx),j=1,ny)
c         read(83,rec=irec2+11) ((sum_sprec_tmp2(i,j),i=1,nx),j=1,ny)
c         read(83,rec=irec1+14) ((sum_smelt_tmp1(i,j),i=1,nx),j=1,ny)
c         read(83,rec=irec2+14) ((sum_smelt_tmp2(i,j),i=1,nx),j=1,ny)

          read(236,rec=irec1) ((swe_tmp(i,j),i=1,nx),j=1,ny)
          read(237,rec=irec1) ((sum_sprec_tmp1(i,j),i=1,nx),j=1,ny)
          read(237,rec=irec2) ((sum_sprec_tmp2(i,j),i=1,nx),j=1,ny)
          read(238,rec=irec1) ((sum_smelt_tmp1(i,j),i=1,nx),j=1,ny)
          read(238,rec=irec2) ((sum_smelt_tmp2(i,j),i=1,nx),j=1,ny)

c For points, just pull the data at the appropriate grid cell.
c   For areas, average the data over the masked out area for each
c   'station'.
          do k=1,nstns
            if (areas_flag.eq.0.0) then
              swe_model(k) = swe_tmp(ii(k),jj(k))
              sumsprec_model(k) = sum_sprec_tmp1(ii(k),jj(k)) -
     &          sum_sprec_tmp2(ii(k),jj(k))
              sumsmelt_model(k) = sum_smelt_tmp1(ii(k),jj(k)) -
     &          sum_smelt_tmp2(ii(k),jj(k))
            elseif (areas_flag.eq.1.0) then
              swe_model(k) = 0.0
              sumsprec_model(k) = 0.0
              sumsmelt_model(k) = 0.0
              swe_count = 0.0
              do j=1,ny
                do i=1,nx
                  if (areas_mask(i,j).eq.obsid(k)) then
c                 if (areas_mask(i,j,iobs_num).eq.obsid(k)) then
                    swe_count = swe_count + 1.0
                    swe_model(k) = swe_model(k) + swe_tmp(i,j)
                    sumsprec_model(k) = sumsprec_model(k) +
     &                sum_sprec_tmp1(i,j) - sum_sprec_tmp2(i,j)
                    sumsmelt_model(k) = sumsmelt_model(k) +
     &                sum_smelt_tmp1(i,j) - sum_smelt_tmp2(i,j)
                  endif
                enddo
              enddo
              swe_model(k) = swe_model(k) / swe_count
              sumsprec_model(k) = sumsprec_model(k) / swe_count
              sumsmelt_model(k) = sumsmelt_model(k) / swe_count
            endif
          enddo
        endif

c To avoid a divide by zero later on, make sure sumsprec_model and
c   sumsmelt_model are not both zero.
        do k=1,nstns
          sumsprec_model(k) = sumsprec_model(k) + 1.0e-6
        enddo

c END USER EDIT SECTION.
cccccccc
ccccccccccccccccc

cccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Determine whether we will adjust the precipitation or melt.  To
c   do this, calculate the relative contributions of precipitation
c   and melt inputs for this correction period.  This can be
c   different for each observation interval.  Calculate the average
c   over all of the stations/areas in the domain.
        sprec_ratio(iobs_num) = 0.0
        smelt_ratio(iobs_num) = 0.0
        do k=1,nstns
          sprec_ratio(iobs_num) = sprec_ratio(iobs_num) +
     &      sumsprec_model(k) / (sumsprec_model(k) + sumsmelt_model(k))
          smelt_ratio(iobs_num) = smelt_ratio(iobs_num) +
     &      sumsmelt_model(k) / (sumsprec_model(k) + sumsmelt_model(k))
        enddo
        sprec_ratio(iobs_num) = sprec_ratio(iobs_num) / real(nstns)
        smelt_ratio(iobs_num) = smelt_ratio(iobs_num) / real(nstns)

c Initialize the delta swe variable.
        if (iobs_num.eq.1) then
          do k=1,nstns
            delta_old(k) = 0.0
          enddo
        else
          do k=1,nstns
            delta_old(k) = 0.0
          enddo
          do k=1,nstns
            do kk=1,nstns_old
              if(obsid(k).eq.obsid_old(kk))
     &          delta_old(k) = delta_old_tmp(kk)
            enddo
          enddo

         write (77,*)
         do k=1,nstns
           write (77,*) 'k, delta_old(k)',k,100.*delta_old(k)
         enddo
         write (77,*)

        endif

c Calculate the correction factor to be used in the next model
c   iteration.  Let the correction factor equal 1.0 during
c   periods where we have no swe observations.  Also, note that the
c   reason for the delta_old variable is to account for the fact
c   that that delta will be fixed with the previous date correction
c   time period.  This is one of the things that allows the
c   correction to be done in two model iterations.
c If sumsprec_model or sumsmelt_model are too small to be used in
c   the assimilation (like less than 1 mm), set corr_factor_tmp = 1.0
c   so no adjustments are performed for this observation interval.
        do k=1,nstns
          if (sprec_ratio(iobs_num).ge.smelt_ratio(iobs_num)) then
            if (sumsprec_model(k).lt.1.0e-3) then
              corr_factor_tmp(k) = 1.0
            else
              corr_factor_tmp(k) = 1.0 +
     &          (swe_obs(k) - swe_model(k) - delta_old(k)) /
     &          sumsprec_model(k)
              corr_factor_tmp(k) = max(cf_min,corr_factor_tmp(k))
            endif
          else
            if (sumsmelt_model(k).lt.1.0e-3) then
              corr_factor_tmp(k) = 1.0
            else
              corr_factor_tmp(k) = 1.0 +
     &          (swe_model(k) - swe_obs(k) + delta_old(k)) /
     &          sumsmelt_model(k)
              corr_factor_tmp(k) = max(cf_min,corr_factor_tmp(k))
            endif
          endif
c TESTING
       write (77,*) '---'
       write (77,*) k,swe_obs(k)
       write (77,*) k,swe_model(k)
       write (77,*) k,delta_old(k)
       write (77,*) k,swe_obs(k)-swe_model(k)-delta_old(k)
       write (77,*) k,sumsprec_model(k)
       write (77,*) k,sumsmelt_model(k)
       write (77,*) k,corr_factor_tmp(k)
       write (77,*) '---'
c TESTING

c Save some data from this observation time for use at the next
c   observation time.
          nstns_old = nstns
          obsid_old(k) = obsid(k)
          delta_old_tmp(k) = swe_obs(k) - swe_model(k)
c         delta_old(k) = swe_obs(k) - swe_model(k)

        enddo

c Now that I have the correction factors calculated at each
c   observation point, interpolate those over the simulation domain.

c Use the barnes oi scheme to create the distribution. If there is
c   only a single station, distribute those data uniformly over
c   the domain.  Make sure that ifill=1, and then undef is not
c   really used (so it does not have to be the same as defined in
c   the .par file).
        undef = -9999.0
        ifill = 1
        iobsint = 0

        if (nstns.ge.2) then
          call get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)

c Modify the size of dn.
          dn = beta * dn

          call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &      nstns,xstn,ystn,corr_factor_tmp,dn,grid,undef,ifill)
        elseif (nstns.eq.1) then
          call single_stn(nx,ny,nstns,corr_factor_tmp,grid)
        endif

c Use the gridded output file to build the corr_factor array.
        do j=1,ny
          do i=1,nx
            corr_factor(i,j,iobs_num) = grid(i,j)
            corr_factor(i,j,iobs_num) =
     &        max(cf_min,corr_factor(i,j,iobs_num))
          enddo
        enddo

c Note that the interpolation scheme may have produced correction
c   factors that do not produce exact matches with the
c   observations (like happens with the case of having a single
c   data point).  If you are interested, calculate the difference
c   between the exact value and the actual calculated value.
c       do k=1,nstns
c         if (sprec_ratio(iobs_num).ge.smelt_ratio(iobs_num)) then
c           corr_offset(k) = sumsprec_model(k) *
c    &        (corr_factor(ii(k),jj(k),iobs_num) - corr_factor_tmp(k))
c         else
c           corr_offset(k) = sumsmelt_model(k) *
c    &        (corr_factor(ii(k),jj(k),iobs_num) - corr_factor_tmp(k))
c         endif
c       enddo

c Write some information to the text file.
        write (77,*) '***************************************'
        write (77,*) ' sprec_ratio =',sprec_ratio(iobs_num),
     &    '  smelt_ratio =',smelt_ratio(iobs_num)
        write (77,*)
        do k=1,nstns
          write (77,*) k,' swe diff =',
     &      100.0*abs(swe_obs(k)-swe_model(k)),' swe obs =',
     &      100.0*swe_obs(k)
          write (77,*) 'sumsprec =',sumsprec_model(k)*100.,
     &      '  swe model =',swe_model(k)*100.
          write (77,*) 'iobs_num =',iobs_num,
     &      '  corr_factor =',corr_factor_tmp(k)
c         write (77,*) 'corr_offset =',100.*corr_offset(k),
c    &      '  ij',ii(k),jj(k)
c         write (77,*) '     delta_old =',100.*delta_old(k),
c    &      '      corr fact used =',corr_factor(ii(k),jj(k),iobs_num)
          write (77,*)
          write (77,*) k,' sumsprec_model(k) =',sumsprec_model(k)
          write (77,*) k,' sumsmelt_model(k) =',sumsmelt_model(k)
          write (77,*)
        enddo
        write (77,*) '***************************************'

c Write the output data to a grads file.
        write(62,rec=iobs_num)
     &    ((corr_factor(i,j,iobs_num),i=1,nx),j=1,ny)

      enddo

c Fill corr_factor with 1.0 for the period following the last
c   obs date.  This is required for the history restart to work
c   correctly.  Without the history restart this was already done
c   as part of the model initialization.
      do j=1,ny
        do i=1,nx
          corr_factor(i,j,nobs_dates+1) = 1.0
        enddo
      enddo

      write(62,rec=nobs_dates+1)
     &  ((corr_factor(i,j,nobs_dates+1),i=1,nx),j=1,ny)

c The met, topo, and veg files must be closed for the next model
c   iteration.
      close (20)
      close (37)
      close (38)

      close (62)

      close (236)
      close (237)
      close (238)

c Build an array indicating the appropriate correction factor to
c   use at any given time during the simulation.
      call corr_factor_index(nobs_dates,icorr_factor_index,
     &  iobs_rec,max_iter,sprec_ratio,smelt_ratio)

      close (77)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine corr_factor_index(nobs_dates,icorr_factor_index,
     &  iobs_rec,max_iter,sprec_ratio,smelt_ratio)

      implicit none

      include 'snowmodel.inc'

      integer icorr_factor_index(max_time_steps)

      integer kk,istart,iend,nobs_dates,iter,max_iter,iprint_inc
      integer iobs_rec(max_obs_dates)
      real sprec_ratio(max_obs_dates),smelt_ratio(max_obs_dates) 

c Define how many model iterations there are in each data write.
c   This should typically be the same as the output print interval
c   (iprint_inc2) defined in outputs_user.f.
      iprint_inc = 24
c     iprint_inc = 1

c Build an array indicating the appropriate correction factor to
c   use at any given time during the simulation.
      do kk=1,nobs_dates+1

c From the simulation start until the first observation.
        if (kk.eq.1) then
          istart = 1
          iend = iobs_rec(kk) * iprint_inc
          if (iend.gt.max_iter) then
            print *,'1 - need to adjust iprint_inc in subroutine'
            print *,'    corr_factor_index in dataassim_user.f'
            stop
          endif
          do iter=istart,iend
            if (sprec_ratio(kk).ge.smelt_ratio(kk)) then
              icorr_factor_index(iter) = 1
            else
              icorr_factor_index(iter) = -1
            endif
          enddo

c Between the last observation and the end of the simulation.
        elseif (kk.eq.nobs_dates+1) then
          istart = iobs_rec(kk-1) * iprint_inc + 1
          iend = max_iter
          if (istart.gt.max_iter) then
            print *,'2 - need to adjust iprint_inc in subroutine'
            print *,'    corr_factor_index in dataassim_user.f'
            stop
          endif
          do iter=istart,iend
            icorr_factor_index(iter) = nobs_dates + 1
          enddo

c Any periods between observations.
        else
          istart = iobs_rec(kk-1) * iprint_inc + 1
          iend = iobs_rec(kk) * iprint_inc
          if (istart.gt.max_iter) then
            print *,'3 - need to adjust iprint_inc in subroutine'
            print *,'    corr_factor_index in dataassim_user.f'
            stop
          endif
          do iter=istart,iend
            if (sprec_ratio(kk).ge.smelt_ratio(kk)) then
              icorr_factor_index(iter) = kk
            else
              icorr_factor_index(iter) = -kk
            endif
          enddo
        endif
      enddo

      do iter=1,max_iter
        write (77,*) iter,icorr_factor_index(iter)
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


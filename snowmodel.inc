c snowmodel.inc

      integer nx_max,ny_max,nstns_max,nvegtypes,max_time_steps,
     &  max_obs_dates

      parameter (nx_max=1811,ny_max=901)
      parameter (nstns_max=10000)
      parameter (nvegtypes=30)
      parameter (max_time_steps=37001)
      parameter (max_obs_dates=12)

c If you are running the multi-layer snow model (even with a single
c   layer) nz_max must be at least one greater than max_layers in
c   snowmodel.par.  This is because the model will build a new layer
c   with the new snowfall and then it is merged with the layer below
c   if you only want a single snow layer.  If you are running
c   SnowModel's original single layer model, nz_max can be 1 (but if
c   nz_max=2 it will avoid a warning message if you are compiling
c   the code with gfortran).
      integer nz_max

c     parameter (nz_max=25)
      parameter (nz_max=2)


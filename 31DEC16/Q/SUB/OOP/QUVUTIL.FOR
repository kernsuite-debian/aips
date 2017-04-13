C   UVDATA Class "Q-routine" utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Q" "UVDATA" utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2001, 2003-2004, 2006-2009, 2012, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Public functions:
C
C   Generic functions:
C   OUVIMG (uvdata, ifield, mfield, image, beam, work1, work2, dobeam,
C      chan, nchavg, imchan, ierr)
C      Make image or beam from a uv data set.
C   UVSUBM (uvin, uvout, ifield, mfield, cname, chanl, nchan, ierr)
C      Generic routine to subtract the FT of a model from a uv data set.
C   UVDIVM (uvin, uvout, mfield, cname, chanl, nchan, ierr)
C      Generic routine to divide the FT of a model into a uv data set.
C   OUVSRT (uvin, uvout, ierr)
C      Sort and optionally rotate uv data.
C   OSDIMG (uvdata, image, ierr)
C      Makes a multi-channel image of the single-dish data contained in
C      UVDATA.
C
C   Specific functions:
C   OUVDFT (uvdata, beam, image, ierr)
C      DFT a uv data set to an image.  High level.
C   ODFT (uv, uvchanm nchav, imchan, chtype, image, beam, factor, ierr)
C      Low level routine to DFT a uv data set.  Called by OUVDFT.
C
C  OOA Fronts to AIPS uv data specific routines:
C   OUNFWT (uv, image, ierr)
C      Applies uniform weighting correction (UVTBUW)
C   OUMSUB (uvin, uvout, ifield, mfield, cname, chanl, nchan, ierr)
C      Subtract the FT of a model from a uv data set; called from
C      UVSUBM.
C   OUMDIV (uvin, uvout, mfield, cname, chanl, nchan, ierr)
C      Divide the FT of a model into a uv data set; called from UVDIVM.
C   OUPDIV (uvin, uvout, mfield, cname, chanl, nchan, ierr)
C      Divide the FT of a polarized model into a uv data set.
C   OUCDFT (uvin, uvout, ifield, nfield, image, chanl, nchan, ierr)
C      Add DFT of "W" corrected model to uv data.
C   OUFDFT (uvin, uvout, nfield, image, chanl, nchan, ierr)
C      Add DFT of bandwidth smearing corrected model to uv data.
C   OUSETF (tflux, nfield, flux)
C      Sets values of total flux densities in DGDS.INC commons
C
C   Semi-public functions:
C   IMPARG (uvdata, avgtim, pangl, zangl, ierr)
C      Determines the observing geometry from the average time.
C   IMTAV (uvdata, avgtim, ierr)
C      Determines the average time of a data set. Averages time in GST
C      and then converts back to Solar time on day 0.  This avoids the
C      problems associated with observations spanning several days.
C      NOTE: this routine uses "AP" memory for scratch memory
C-----------------------------------------------------------------------
      SUBROUTINE UVSUBM (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL,
     *   NCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Generic routine to subtract the Fourier transform of an image model
C   from a uv data set. The appropriate routine is determined from the
C   presence and values of the following keywords on the input uv data.
C      DOPBFM   L   If present and true make frequency dependent primary
C                   beam corrections using PBFSUB ($QOOP/PBUTIL.FOR).
C                   Note: This cannot do 'DFT' imaging.
C   and attached to IMAGE(1):
C      FTTYPE    C*4 Fourier transform type 'FFT' or 'DFT'. ('FFT')
C                    For 'FFT' OUMSUB is called
C                    For 'DFT' OUCDFT is called.
C                    (Note some restrictions apply)
C   Can use either a set of clean components or an image.
C      Note: if the FT of an image is to be used then it should be the
C   first or only plane in the image(s).
C   Inputs:
C      UVIN     C*?      Name of input uvdata object.
C      UVOUT    C*?      Name of output uvdata object, will be created
C                        if necessary as a scratch file.
C      IFIELD   I        Field to do; 0 => all
C      NFIELD   I        Number of fields
C      IMAGE    C(*)*?   array of image names to subtract.
C      CHANL    I        First channel in uv data to process
C      NCHAN    I        Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R(2)  Model, data factors (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model flux in Jy, (0.0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INTEGER   MSGSAV, TYPE, DIM(7), DUMMY
      LOGICAL   DOPBFM
      REAL      MODFAC(2)
      CHARACTER FTTYPE*4, CDUMMY*1, SPIX*32
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      MSGSAV = MSGSUP
C                                       Check for DOPBFM
      MSGSUP = 32000
C                                       Default = false
      CALL OUVGET (UVIN, 'DOPBFM', TYPE, DIM, DOPBFM, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Check for SPIX corr
      CALL OUVGET (UVIN, 'SPIXIMAGE', TYPE, DIM, DUMMY, SPIX, IERR)
      IF (IERR.EQ.1) THEN
         SPIX = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       FTTYPE
C                                       Default = FFT
      CALL IMGET (IMAGE(1), 'FTTYPE', TYPE, DIM, DUMMY, FTTYPE, IERR)
      IF (IERR.EQ.1) THEN
         FTTYPE = 'FFT'
         IERR = 0
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
C                                       FLDSZ needed
      CALL IMGET (IMAGE(1), 'IMSIZE', TYPE, DIM, FLDSZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Call appropriate routine
C                                       Frequency dependent primary beam
C                                       or spectral index corrections.
      IF ((DOPBFM) .OR. (SPIX.NE.' ')) THEN
         CALL PBFSUB (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL, NCHAN,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE IF (FTTYPE.EQ.'FFT') THEN
         CALL OUMSUB (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL, NCHAN,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE IF (FTTYPE.EQ.'DFT') THEN
C                                       Have to flip sign of FACTOR
         MSGSUP = 32000
         CALL OUVGET (UVIN, 'MODFACT', TYPE, DIM, MODFAC, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            MODFAC(1) = 1.0
            MODFAC(2) = 1.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 995
         IF (ABS (MODFAC(1)) .LT. 1.0E-10) MODFAC(1) = 1.0
         MSGSUP = MSGSAV
         MODFAC(1) = -MODFAC(1)
         DIM(1) = 2
         DIM(2) = 1
         CALL OUVPUT (UVIN, 'MODFACT', OOARE, DIM, MODFAC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Do model
         CALL OUCDFT (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL, NCHAN,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Flip it back
         MODFAC(1) = -MODFAC(1)
         DIM(1) = 2
         DIM(2) = 1
         CALL OUVPUT (UVIN, 'MODFACT', OOARE, DIM, MODFAC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Huh?
      ELSE
         MSGTXT = 'UVSUBM: UNKNOWN FTTYPE ' // FTTYPE
         IERR = 5
         GO TO 995
         END IF
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      MSGTXT = 'UVSUBM: ERROR SUBTRACTING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'UVSUBM: FROM ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE UVDIVM (UVIN, UVOUT, NFIELD, IMAGE, CHANL, NCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Generic routine to divide the Fourier transform of an image model
C   into a uv data set. The appropriate routine is determined from the
C   presence and values of the following keywords on the input uv data:
C      DOPBFM   L   If present and true make frequency dependent primary
C                   beam corrections using PBFDIV ($QOOP/PBUTIL.FOR).
C   Note: This cannot do 'DFT' imaging.
C   Can use either a set of clean components or an image.
C      Note: if the FT of an image is to be used then it should be the
C   first or only plane in the image(s).
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      NFIELD  I    Number of fields
C      IMAGE   C(*)*? array of image names to subtract.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R(2)  Model, data factors (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model flux in Jy, (0.0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INTEGER   MSGSAV, TYPE, DIM(7), DUMMY
      LOGICAL   DOPBFM
      CHARACTER FTTYPE*4, CDUMMY*1, SPIX*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      MSGSAV = MSGSUP
C                                       Check for DOPBFM
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'DOPBFM', TYPE, DIM, DOPBFM, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         IERR = 0
         END IF
C                                       Check for SPIX corr
      CALL OUVGET (UVIN, 'SPIXIMAGE', TYPE, DIM, DUMMY, SPIX, IERR)
      IF (IERR.EQ.1) THEN
         SPIX = ' '
         IERR = 0
         END IF
C                                       FTTYPE
      CALL IMGET (IMAGE(1), 'FTTYPE', TYPE, DIM, DUMMY, FTTYPE, IERR)
      IF (IERR.EQ.1) THEN
         FTTYPE = 'FFT'
         IERR = 0
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
C                                       FLDSZ needed
      CALL IMGET (IMAGE(1), 'IMSIZE', TYPE, DIM, FLDSZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Call appropriate routine
C                                       Frequency dependent primary beam
C                                       or spectral index corrections.
      IF ((DOPBFM) .OR. (SPIX.NE.' ')) THEN
         CALL PBFDIV (UVIN, UVOUT, NFIELD, IMAGE, CHANL, NCHAN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE IF (FTTYPE.EQ.'FFT') THEN
         CALL OUMDIV (UVIN, UVOUT, NFIELD, IMAGE, CHANL, NCHAN, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       No can do
      ELSE IF (FTTYPE.EQ.'DFT') THEN
         MSGTXT = 'UVDIVM: CANNOT HANDLE DFT IMAGING'
         IERR = 5
         GO TO 995
C                                       Huh?
      ELSE
         MSGTXT = 'UVDIVM: UNKNOWN FTTYPE ' // FTTYPE
         IERR = 5
         GO TO 995
         END IF
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      MSGTXT = 'UVDIVM: ERROR DIVIDING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'UVDIVM: INTO ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUVDFT (UVDATA, BEAM, IMAGE, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   DFT a uv data set to an image.
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C   Output:
C      BEAM    C*?  Name of beam object.
C      IMAGE   C*?  Name of Image object
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), BEAM*(*), IMAGE*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), CHINC, BCHAN, ECHAN, NCHAV, ICHAN, IMCHAN,
     *   NUMCH, LECHAN
      CHARACTER UVTMP*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
C                                       Specify output image and beam
C                                       Copy object descriptors
      CALL U2IDES (UVDATA, BEAM, .TRUE., IERR)
      IF (IERR.NE.0) GO TO 990
      CALL U2IDES (UVDATA, IMAGE, .TRUE., IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Array descriptor and axis labels
C                                       Select data to a temporary file
      UVTMP = 'Scratch uv data file for OUVDFT'
      CALL UV2SCR (UVDATA, UVTMP, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Control info
C                                       Find number of channels
      CALL OGET (UVDATA, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVDATA, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BCHAN = MAX (1, BCHAN)
      ECHAN = MAX (BCHAN, ECHAN)
      CALL OGET (UVDATA, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CHINC = MAX (1, CHINC)
      CALL OGET (UVDATA, 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      NCHAV = MAX (1, NCHAV)
      NUMCH = (ECHAN - BCHAN + 1 - NCHAV) / CHINC + 1
      LECHAN = BCHAN + CHINC*NUMCH - 1
      CALL OPUT (UVDATA, 'CALEDIT.BCHAN', OOAINT, DIM, BCHAN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVDATA, 'CALEDIT.ECHAN', OOAINT, DIM, ECHAN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Image
      IMCHAN = 0
      DO 20 ICHAN = BCHAN,LECHAN,CHINC
C                                       Uniform weighting
         CALL OUNFWT (UVTMP, IMAGE, IERR)
         IF (IERR.NE.0) GO TO 990
         IMCHAN = IMCHAN + 1
         CALL ODFT (UVTMP, ICHAN, NCHAV, IMCHAN, IMAGE, BEAM, .TRUE.,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
 20      CONTINUE
C                                       Destroy temporary uv data
      CALL OUVZAP (UVTMP, IERR)
      GO TO 999
C                                       Error
 990  MSGTXT = 'OUVDFT: ERROR IMAGING ' // UVDATA
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE ODFT (UV, UVCHAN, NCHA, IMCHAN, IMAGE, BEAM, DOBEAM,
     *   IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Low level routine to DFT a uv data set.  Called by OUVDFT, OUVIMG
C   Makes one image
C   Inputs:
C      UV      C*?  Name of uvdata object.
C      UVCHAN  I    initial channel number in UV data set
C      NCHA    I    Number channels averaged
C      IMCHAN  I    Channel in output image
C      IMAGE   C*?  Name of Image object.
C      BEAM    C*?  Name of Beam object.
C      DOBEAM  L    If true make beam, else image
C   Inputs from IMAGE object:
C      IMSIZE   I(2) Image size in pixels
C      CELLSIZE R(2) Cell size in arc seconds.
C      CHTYPE   C*4  'LINE' or 'SUM' (default 'SUM')
C      TAPER    R(2) X and Y taper values
C      XSHIFT   R    X position shift
C      YSHIFT   R    Y position shift
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      CHARACTER UV*(*), IMAGE*(*), BEAM*(*)
      LOGICAL   DOBEAM
      INTEGER   UVCHAN, IMCHAN, NCHA, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PREMIN*5, PREMAX*5, CDUMMY*1, UVTYPE*2
      INTEGER   TYPE, DIM(7), LREC, NRPARM, INCFQ, NVSLOD, NVIS,
     *   NAXIS(7), NCHIF, NCOPY, I1, I2, I3, I, LERR, MWORD, DUMMY,
     *   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, JLOCC,
     *   JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF, IAPDAT,
     *   IAPUV, IAPVIS, IAPWT, LVIS, IAPMAP, IAPBEM, IMSIZE(2), LOOPV,
     *   NVLOAD, PLANE(7), MSGSAV, TOTVIS, ILOCA1, ILOCA2, ILOCSA
      REAL      CELSIZ(2), TAPER(2), FACTOR, SSROT, CCROT, MAXP, MINP,
     *   APCONS(MAXCIF+20), IMROTA(7), IMXSH, IMYSH,  UVXSH, UVYSH, DXC,
     *   DYC, DZC
      DOUBLE PRECISION FREQUV, FREQS(MAXCIF), RAOFF, DECOFF, RA, DEC,
     *   RAX, DECX, CRVAL(7), ICRVAL(7), IMRA, IMDEC
      LOGICAL   APOPEN
      SAVE APCONS, FREQS
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA PLANE /7*1/
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      MSGSAV = MSGSUP
      APOPEN = .FALSE.
C                                       Open objects
      CALL OUVOPN (UV, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UV, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'ODFT DOES NOT WORK FOR UV DATA OF TYPE ''' //
     *      UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
C                                       open beam and image objects
      CALL IMGOPN (IMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOBEAM) THEN
         CALL IMGOPN (BEAM, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Uv data pointers
      CALL UVDPNT (UV, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       LREC
      CALL UVDGET (UV, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UV, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of visibilities
      CALL UVDGET (UV, 'GCOUNT', TYPE, DIM, NVIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NAXIS
      CALL UVDGET (UV, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRVAL
      CALL UVDGET (UV, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Frequencies
      CALL UVFRQS (UV, FREQUV, FREQS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of channels-IFs
      NCHIF = NAXIS(JLOCF)
      IF (JLOCIF.GT.0) NCHIF = NCHIF * NAXIS(JLOCIF)
C                                       Info from IMAGE and UV objects
C                                       Image size
      CALL IMGET (IMAGE, 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Cellsize
      CALL IMGET (IMAGE, 'CELLSIZE', TYPE, DIM, CELSIZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRVAL
      CALL IMDGET (IMAGE, 'CRVAL', TYPE, DIM, ICRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Image rotation
      CALL IMDGET (IMAGE, 'CROTA', TYPE, DIM, IMROTA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Position shifts
      CALL PSNGET (IMAGE, 'XSHIFT', TYPE, DIM, IMXSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNGET (IMAGE, 'YSHIFT', TYPE, DIM, IMYSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNGET (UV, 'XSHIFT', TYPE, DIM, UVXSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNGET (UV, 'YSHIFT', TYPE, DIM, UVYSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Taper
      CALL IMGET (IMAGE, 'UVTAPER', TYPE, DIM, TAPER, CDUMMY, IERR)
C                                       Default = none
      IF (IERR.EQ.1) THEN
         TAPER(1) = 0.0
         TAPER(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Shift
      MSGSUP = MSGSAV
      RA = CRVAL(JLOCR)
      DEC = CRVAL(JLOCD)
      IMRA = ICRVAL(1) + IMXSH
      IMDEC = ICRVAL(2) + IMYSH
      RAX = RA + UVXSH
      DECX = DEC + UVYSH
      RAOFF = SIN (DG2RAD * (IMRA - RAX)) * COS (IMDEC * DG2RAD)
      DECOFF = (COS (DECX * DG2RAD) * SIN (IMDEC * DG2RAD) -
     *   SIN (DECX * DG2RAD) * COS (IMDEC * DG2RAD) *
     *   COS ((IMRA - RAX) * DG2RAD))
      CCROT = COS (DG2RAD * IMROTA(2))
      SSROT = SIN (DG2RAD * IMROTA(2))
      DXC =  - (RAOFF * CCROT - DECOFF * SSROT)
      DYC =  - (DECOFF * CCROT + RAOFF * SSROT)
      DZC = - (SQRT (1.0D0 - DXC*DXC - DYC*DYC)  - 1.0D0)
C                                       "grab AP"
      I1 = 2 * IMSIZE(1) * IMSIZE(2) + 100 * LREC
      I1 = I1 / 1024 + 2
      CALL QINIT (I1, I2, I3)
      IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'ODFT: FAILED TO GET NEEDED MEMORY'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 990
         END IF
      CALL APOBJ ('OPEN', 'ODFT', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
C                                       Visibility count
      TOTVIS = 0
C                                       Setup for gridding
C                                       Taper
      APCONS(9) = 0.0
      APCONS(10) = 0.0
      IF (TAPER(1).GT.0.) APCONS(9) = LOG(.3) /
     *   ((TAPER(1) * 1E3) ** 2)
      IF (TAPER(2).GT.0.) APCONS(10) = LOG(.3) /
     *   ((TAPER(2) * 1E3) ** 2)
C                                       Flip sign on v to make maps come
C                                       out upside down.
      APCONS(11) = CELSIZ(1) / 2.06264E5
      APCONS(12) = - ABS (CELSIZ(2)) / 2.06264E5
C                                       Shift parameters
      APCONS(14) = DXC
      APCONS(15) = DYC
      APCONS(16) = DZC
C                                       Center pixel
      APCONS(17) = IMSIZE(1) / 2
      APCONS(18) = IMSIZE(2) / 2 + 1
C                                       Frequency scaling table
      APCONS(20) = (FREQS(UVCHAN) / FREQUV) - 1.0D0
      DO 100 I = 2,NCHA
         APCONS(19+I) = (FREQS(UVCHAN+I-1) / FREQS(UVCHAN+I-2)) - 1.0D0
 100     CONTINUE
C                                       Setup AP
      NCOPY = 19 + NCHA
      CALL QPUT (APCONS, 1, NCOPY, 2)
C                                       AP addresses
      LVIS = LREC
      INCFQ = MIN (INCF, INCIF)
      MWORD = IMSIZE(1) * IMSIZE(2)
      IAPDAT = 10 + NCOPY
      IAPUV = IAPDAT - ILOCU + 1
      IAPVIS = IAPDAT + NRPARM
      IAPWT = IAPVIS + 2
C                                       Image and beam at top of memory
      IAPMAP = ((PSAPNW * 1024) - 1) - MWORD
      IAPBEM = IAPMAP - MWORD - 1
C                                       How many VIS can be loaded?
      NVSLOD = (IAPBEM - IAPDAT) / LVIS
C                                       Does this fit?
      IF (NVSLOD.LT.1) THEN
         IERR = 9
         MSGTXT = 'ODFT: AP MEMORY TOO SMALL'
         GO TO 995
         END IF
C                                       Clear IMAGE, BEAM in "AP"
      CALL QVCLR (IAPMAP, 1, MWORD)
      CALL QVCLR (IAPBEM, 1, MWORD)
C                                       Image
      DO 500 LOOPV = 1,NVIS,NVSLOD
         NVLOAD = NVIS - LOOPV +1
         NVLOAD = MIN (NVLOAD, NVSLOD)
C                                       Load vis
         CALL UV2AP (UV, IAPDAT, NVLOAD, LVIS, NRPARM, IERR)
         IF (IERR.NE.0) GO TO 990
         TOTVIS = TOTVIS + NVLOAD
C                                       Grid
         CALL QDFT (IAPUV, IAPVIS, IAPWT, LVIS, INCFQ, NVLOAD, NCHA,
     *      IMSIZE(1), IMSIZE(2), IAPMAP, IAPBEM)
 500     CONTINUE
C                                       Normalize by peak of beam.
      CALL QMAXV (IAPBEM, 1, 0, MWORD)
      CALL QGET (FACTOR, 0, 1, 2)
      IF (FACTOR.GT.1.0E-20) THEN
         FACTOR = 1.0 / FACTOR
      ELSE
         FACTOR = 1.0
         END IF
      CALL QPUT (FACTOR, 0, 1, 2)
      IF (DOBEAM) CALL QVSMUL (IAPBEM, 1, 0, IAPBEM, 1, MWORD)
      CALL QVSMUL (IAPMAP, 1, 0, IAPMAP, 1, MWORD)
C                                       Copy to image
      PLANE(3) = IMCHAN
      CALL AP2IMG (IAPMAP, IMAGE, PLANE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOBEAM) THEN
         CALL AP2IMG (IAPBEM, BEAM, PLANE, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Tell what's been done
      IF (DOBEAM) THEN
         WRITE (MSGTXT,1800) IMCHAN, TOTVIS
         CALL MSGWRT (4)
         WRITE (MSGTXT,1801) FACTOR
         CALL MSGWRT (4)
C                                       Max,min
         CALL ARSGET (BEAM, 'DATAMAX', TYPE, DIM, MAXP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ARSGET (BEAM, 'DATAMIN', TYPE, DIM, MINP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL METSCA (MAXP, PREMAX, LERR)
         CALL METSCA (MINP, PREMIN, LERR)
         WRITE (MSGTXT,1802) MINP, PREMIN, MAXP, PREMAX
         CALL MSGWRT (4)
      ELSE
C                                       Max,min
         CALL ARSGET (IMAGE, 'DATAMAX', TYPE, DIM, MAXP, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ARSGET (IMAGE, 'DATAMIN', TYPE, DIM, MINP, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL METSCA (MAXP, PREMAX, LERR)
         CALL METSCA (MINP, PREMIN, LERR)
         WRITE (MSGTXT,1803) MINP, PREMIN, MAXP, PREMAX
         CALL MSGWRT (4)
         END IF
C                                       Release "AP"
      CALL QRLSE
      CALL APOBJ ('FREE', 'ODFT', LERR)
      APOPEN = .FALSE.
C                                       Close object
      CALL IMGCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOBEAM) THEN
         CALL IMGCLO (BEAM, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL OUVCLO (UV, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'ODFT', LERR)
         END IF
      MSGTXT = 'ODFT: ERROR IMAGING ' // UV
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('ODFT: Channel ',I5, ' used ', I9, ' vis')
 1801 FORMAT ('     Beam scaling factor = ', 1PE12.5)
 1802 FORMAT ('Beam min = ',F6.1,1X,A5,'Jy, max = ',F6.1,1X,A5,'Jy')
 1803 FORMAT ('Field 1  min = ',F6.1,1X,A5,'Jy',
     *   ',max = ',F6.1,1X,A5,'Jy')
      END
      SUBROUTINE OUNFWT (UV, IMAGE, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Applies uniform weighting correction (UVTBUW) if selected.
C   Note: this routine weights by the sum of the weights in a weighting
C   box rather than the number of counts.
C   Inputs:
C      UV      C*?  Name of uvdata.
C      IMAGE   C*?  Name of image defining grid size and
C   Inputs from IMAGE: (defaults where possible)
C      CHINC   I    Channel increment (1)
C      CHTYPE  C*4  'LINE' or 'SUM ' ('SUM ')
C      UVWTFN  C*2  Uniform weighting option ('UN')
C      UVBOX   I    Uniform weighting box (0,0)
C      NX      I    Number of pixels in X direction.
C      NY      I    Number of pixels in Y direction.
C      CELLS   R(2) Cellspacing in x,y in arcsec
C      NCHAV   I    Number of channels to be averaged.
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      CHARACTER UV*(*), IMAGE*(*)
      INTEGER   IERR
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, MSGSAV, SCRNO,
     *   TYPE, DIM(7), DSK, CNO, CHINC, NAXIS(7), NCHAV, BIF, EIF, LERR,
     *   BCHAN, ECHAN, LLOCF, LLOCIF, NAXUV(7), DUMMY, NUVXIS(2),
     *   BUFNO3, LUNST(3), LLREC, JBUFSZ, NEED, I1, I2, I3
      LOGICAL   ISSCR, APOPEN
      REAL      AXINC(7), MAXUU, UVTAPR(2)
      CHARACTER TEMPUV*32, TEMPIM*32, CHTYPE*4, UVWTFN*2, STOKES*4,
     *   CDUMMY*1, SORD*2
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION FREQS(MAXCIF)
      SAVE FREQS
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      MSGSAV = MSGSUP
      APOPEN = .FALSE.
C                                       Stuff in common
C                                       Control info
C                                       UVTAPER
      MSGSUP = 32000
      CALL IMGET (IMAGE, 'WTTAPER', TYPE, DIM, UVTAPR, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         UVTAPR(1) = 0.0
         UVTAPR(2) = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
      TAPERU = MAX (0.0, UVTAPR(1))
      TAPERV = MAX (0.0, UVTAPR(2))
C                                       UVWTFN
      MSGSUP = 32000
      CALL IMGET (IMAGE, 'UVWTFN', TYPE, DIM, DUMMY, UVWTFN, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         UVWTFN = 'UN'
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (UVWTFN.EQ.'  ') UVWTFN = 'UN'
      WTPOWR = 1.0
      IF (UVWTFN(2:2).EQ.'S') WTPOWR = 0.50
      IF (UVWTFN(2:2).EQ.'V') WTPOWR = 0.25
      IF (UVWTFN(2:2).EQ.'O') WTPOWR = 0.00
      IF (UVWTFN(1:1).EQ.'C') WTPOWR = -WTPOWR
C                                       Weighting requested?
      DOUNIF = (UVWTFN(1:1).NE.'N')
      IF ((.NOT.DOUNIF) .AND. (WTPOWR.EQ.1.0) .AND. (TAPERU.LE.0.0)
     *   .AND. (TAPERV.LE.0.0)) GO TO 999
C                                       Max value abs(U)
      MSGSUP = 32000
      CALL OUVGET (UV, 'MAXBLINE', TYPE, DIM, MAXUU, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         MAXUU = -1.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       CHINC
      MSGSUP = 32000
      CALL OUVGET (UV, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CHINC = 1
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (CHINC.LE.0) CHINC = 1
C                                       Line or summing channels?
      CALL IMGET (IMAGE, 'CHTYPE', TYPE, DIM, DUMMY, CHTYPE, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CHTYPE = 'SUM '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       UVBOX
      CALL IMGET (IMAGE, 'UVBOX', TYPE, DIM, UNFBOX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         UNFBOX = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       UVBXFN
      CALL IMGET (IMAGE, 'UVBXFN', TYPE, DIM, NBXFUN, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NBXFUN = 1
         END IF
      IF (IERR.NE.0) GO TO 995
      IF ((NBXFUN.GT.4) .OR. (NBXFUN.EQ.0)) NBXFUN = 1
      NBXFUN = MAX (NBXFUN, -4)
C                                       ROBUST
      CALL IMGET (IMAGE, 'ROBUST', TYPE, DIM, ROBUST, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         IF (DOUNIF) THEN
            ROBUST = -7.0
         ELSE
            ROBUST = 7.0
            END IF
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (ROBUST.GT.7.0) THEN
         DOUNIF = .FALSE.
         IF ((.NOT.DOUNIF) .AND. (WTPOWR.EQ.1.0) .AND. (TAPERU.LE.0.0)
     *      .AND. (TAPERV.LE.0.0)) GO TO 999
         END IF
C                                       Number of channels to average
      MSGSUP = 32000
      CALL IMGET (IMAGE, 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         NCHAV = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Image size
      CALL ARDGET (IMAGE, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UV weight size
      CALL IMGET (IMAGE, 'UVSIZE', TYPE, DIM, NUVXIS, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NUVXIS(1) = NAXIS(1)
         NUVXIS(2) = NAXIS(2)
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (NUVXIS(1).LE.0) NUVXIS(1) = NAXIS(1)
      IF (NUVXIS(2).LE.0) NUVXIS(2) = NAXIS(2)
C                                       LREC
      CALL UVDGET (UV, 'LREC', TYPE, DIM, LLREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Cellsize
      CALL IMDGET (IMAGE, 'CDELT', TYPE, DIM, AXINC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Selection.
      CALL SECSLT (UV, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Axis indices
      CALL UVDGET (UV, 'NAXIS', TYPE, DIM, NAXUV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UV, 2, 'FREQ', LLOCF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       IF may not be present
      CALL UVDFND (UV, 2, 'IF  ', LLOCIF, IERR)
      IF (IERR.NE.0) LLOCIF = -1
C                                       Defaults
      IF (BIF.LE.0) BIF = 1
      IF ((EIF.LE.0) .AND. (LLOCIF.GT.0)) EIF = NAXUV(LLOCIF)
      EIF = MAX (EIF, 1)
      IF (BCHAN.LE.0) BCHAN = 1
      IF (ECHAN.LE.0) ECHAN = NAXUV(LLOCF)
      IF (NCHAV.LE.0) THEN
         IF (CHTYPE.EQ.'LINE') THEN
            NCHAV = 1
         ELSE
            NCHAV = ECHAN - BCHAN + 1
            NCHAV = MAX (1, NCHAV) * (EIF - BIF + 1)
            END IF
         END IF
      NCHAVG = NCHAV
      CHUV1 = 1
      NGRDAT = .TRUE.
      CELLSG(1) = AXINC(1) * 3600.0
      CELLSG(2) = AXINC(2) * 3600.0
      NXUNF = NUVXIS(1)
      NYUNF = NUVXIS(2)
C                                       Frequencies
      CALL OUVOPN (UV, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVFRQS (UV, FREQUV, FREQG, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UV, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Disk, CNO
      CALL OBDSKC (UV, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
      DISKO = DISKI
      CNOO = CNOI
C                                       CFIL.INC scratch file number.
      MSGSUP = 32000
      SCRNO = 0
      CALL OUVGET (UV, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (SCRNO.LE.0)) THEN
         IERR = 0
         SCRNO = -1
         END IF
C                                       Not there, ISSCR = false
      IF (IERR.NE.0) GO TO 995
      ISSCR = SCRNO .GT. 0
C                                       Permanent or scratch file?
C                                       Scratch
      IF (ISSCR) THEN
         DSK = 0
         CNO = SCRNO
C                                       Permanent
      ELSE
         DSK = DISKI
         CNO = CNOI
         END IF
C                                       Decide about sorting
      MSGSUP = 32000
      CALL GRDFIT (1, LLREC, FREQG, MAXUU, '    ', NEED, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.LT.0) .OR. (IERR.EQ.1)) THEN
         CALL QRLSE
         I1 = NEED / 1024 + 10
         CALL QINIT (I1, I2, I3)
         IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'OUNFWT: FAILED TO GET NEEDED MEMORY'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 990
            END IF
         CALL GRDFIT (1, LLREC, FREQG, MAXUU, '    ', NEED, IERR)
         END IF
      IF (IERR.GT.1) THEN
         GO TO 990
      ELSE IF (IERR.EQ.1) THEN
         MSGTXT = 'PROBLEM AS POSED WILL NOT FIT EVEN IF DATA ARE'
     *      // ' SORTED'
         GO TO 995
C                                       Do sort
      ELSE IF (IERR.LT.0) THEN
         CALL UVDGET (UV, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (SORD(1:1).NE.'X') THEN
            MSGTXT = 'OUNFWT: Sorting data to make them fit'
            CALL MSGWRT (2)
            SORD = 'XY'
            CALL OUVPUT (UV, 'SORT', OOACAR, DIM, DUMMY, SORD, IERR)
            IF (IERR.NE.0) GO TO 990
            DIM(1) = 1
            CALL OUVPUT (UV, 'ROTATE', OOARE, DIM, 0.0, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OUVSRT (UV, UV, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      MSGSUP = MSGSAV
C                                       Get catblk
      CALL OUVCGT (UV, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Make temporary object for buffer
      TEMPUV = 'Temporary uv data for OUNFWT'
      CALL OBCREA (TEMPUV, 'UVDATA  ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Make temporary object for buffer
      TEMPIM = 'Temporary weight grid data for OUNFWT'
      CALL OBCREA (TEMPIM, 'IMAGE   ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open for buffers
      CALL OBOPEN (UV, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBOPEN (TEMPIM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get buffer numbers
      CALL OBINFO (UV, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPUV, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPIM, BUFNO3, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUN (LUNST(1), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUN (LUNST(2), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUN (LUNST(3), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       get freqs
C                                       Frequencies
      CALL UVFRQS (UV, FREQUV, FREQS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       open AP
      CALL APOBJ ('OPEN', 'OUNFWT', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
C                                       Do uniform weighting
      JBUFSZ = BUFSIZ * 2
      CALL UVWAIT (DSK, CNO, CATBLK, LUNST, JBUFSZ, FREQS,
     *   OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), OBUFFR(1,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL APOBJ ('FREE', 'OUNFWT', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .FALSE.
      CALL OBLUFR (LUNST(1), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUFR (LUNST(2), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUFR (LUNST(3), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (UV, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPIM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary object
      CALL OBFREE (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBFREE (TEMPIM, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUNFWT', LERR)
         END IF
      MSGTXT = 'OUNFWT: ERROR IN UNIFORM WEIGHTING OF ' // UV
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUMSUB (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL,
     *   NCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Subtract the Fourier transform of an image model from a uv data set.
C   Can use either a set of clean components or an image.
C      Note: if the FT of an image is to be used then it should be the
C   first or only plane in the image(s).
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      IFIELD  I    Field to do; 0 => all
C      NFIELD  I    Number of fields
C      IMAGE   C(*)*? array of image names to subtract.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R(2)  Model, data factors (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model flux in Jy, (0.0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C      MODPARMS  R(4)  Point model: extended object parameters
C                       1=> model type, 0=point, 1=gaussian, 2,3=sphere
C                       Gaussian: (2)=major axis(asec), (3)=minor axis
C                                 (4)=PA (degrees)
C                       Sphere: (2)=radius (asec).
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, BUFNO3, LERR,
     *   MSGSAV, SCRNO, TYPE, DIM(7), MODEL, METHOD, CHANEL, MCHAN,
     *   JBUFSZ, I, NS, FREQID, COUNT, BIF, EIF, BCHAN, ECHAN, NCCB,
     *   DUMMY, LFIELD
      LOGICAL   DOSUM, DOMSG, GETNCC, NEWOUT, INOUT, APOPEN
      CHARACTER TEMPUV*32, TEMP2*32, TMPTAB*32, CMOD*4, CMET*4,
     *   STOKES*4, SORD*2, CDUMMY*1, UVTYPE*2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSCD.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      COMPDT = .FALSE.
      DATDIV = .FALSE.
      APOPEN = .FALSE.
      IERR = 0
C                                       Is input = output?
      INOUT = UVIN.EQ.UVOUT
      MSGSAV = MSGSUP
C                                       Disk, CNO
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save and reset selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output may not exist
      MSGSUP = 32000
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DISKO = 0
         CNOO = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       DFIL.INC scratch file number.
      SCRNO = 0
      CALL OUVGET (UVIN, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
C                                       Scratch file
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
         DISKI = 0
         CNOI = SCRNO
C                                       Clear any flags
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (UVIN, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
         IERR = 0
         CALL OUCCLR (UVIN, IERR)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Output
      SCRNO = 0
      MSGSUP = 32000
      CALL OUVGET (UVOUT, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
C                                       Scratch file
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
         DISKO = 0
         CNOO = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Does output exist
      NEWOUT = (DISKO.LE.0) .AND. (CNOO.LE.0)
C                                       Make temporary objects for
C                                       buffer
      TEMPUV = 'Temporary uv data 1 for OUMSUB'
      CALL OBCREA (TEMPUV, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         TEMP2 = 'Temporary uv data 2 for OUMSUB'
         CALL OBCREA (TEMP2, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open for buffers, update info
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'OUMSUB DOES NOT WORK FOR UV DATA OF TYPE ''' //
     *      UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBOPEN (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Save vis count.
      ELSE
         CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       This funny business to fool the
C                                       I/O which thinks no data will be
C                                       written.
         CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVOUT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get buffer numbers
      CALL OBINFO (UVIN, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPUV, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBINFO (TEMP2, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OBINFO (UVOUT, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Set values in common
      MFIELD = NFIELD
      MSGSUP = 32000
C                                       CC version default = 1
      CALL OUVGET (UVIN, 'MODCCVER', TYPE, DIM, CCVER, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (MAXFLD, 1, CCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCBEG', TYPE, DIM, NSUBG, CDUMMY, IERR)
      NCCB = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCB = MAXFLD
         IERR = 0
         CALL FILL (NCCB, 1, NSUBG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Highest component
      CALL OUVGET (UVIN, 'MODCCEND', TYPE, DIM, NCLNG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NS = MAXFLD
         CALL FILL (NS, 0, NCLNG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Defaulted no. components?
      GETNCC = .TRUE.
      DO 20 I = 1,NFIELD
         IF (NCLNG(I).GT.0) GETNCC = .FALSE.
 20      CONTINUE
C                                       Do negative?
      CALL OUVGET (UVIN, 'MODNONEG', TYPE, DIM, NONEG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NONEG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Flux cutoff
      CALL OUVGET (UVIN, 'MODFLUX', TYPE, DIM, LIMFLX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         LIMFLX = -1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model type
      CALL OUVGET (UVIN, 'MODMODEL', TYPE, DIM, DUMMY, CMOD, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMOD = 'CC  '
         END IF
      IF (IERR.NE.0) GO TO 995
      GETNCC = GETNCC .AND. (CMOD.EQ.'CC  ')
C                                       Model method
      CALL OUVGET (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMET = 'DFT '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model Factor
      CALL OUVGET (UVIN, 'MODFACT ', TYPE, DIM, FACGRD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FACGRD(1) = 1.0
         FACGRD(2) = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Progress report?
      CALL OUVGET (UVIN, 'MODDOMSG', TYPE, DIM, DOMSG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOMSG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Point model
      CALL OUVGET (UVIN, 'MODDOPT ', TYPE, DIM, DOPTMD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOPTMD = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (DOPTMD) THEN
C                                       Flux
         CALL OUVGET (UVIN, 'MODPTFLX ', TYPE, DIM, PTFLX, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTFLX = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X offset
         CALL OUVGET (UVIN, 'MODPTXOF ', TYPE, DIM, PTRAOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTRAOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y offset
         CALL OUVGET (UVIN, 'MODPTYOF ', TYPE, DIM, PTDCOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTDCOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       extended object parameters
         CALL OUVGET (UVIN, 'MODPARMS ', TYPE, DIM, PARMOD, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PARMOD(1) = 0.0
            PARMOD(2) = 0.0
            PARMOD(3) = 0.0
            PARMOD(4) = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Use DFT for point model.
         CMET = 'DFT '
C                                       Disk numbers, no. CC
C                                       Loop over fields
      ELSE
         MSGSUP = MSGSAV
C                                       3D imaging requires all set
         DO 50 LFIELD = 1,NFIELD
            CALL OBDSKC (IMAGE(LFIELD),  CCDISK(LFIELD), CCCNO(LFIELD),
     *         IERR)
            IF (IERR.NE.0) GO TO 990
 50         CONTINUE
         IF (GETNCC) THEN
            TMPTAB = 'Temporary table for OUMSUB'
            DO 60 LFIELD = 1,NFIELD
C                                       Number of components
               CALL IM2TAB (IMAGE(LFIELD), TMPTAB, 'CC',
     *            ABS(CCVER(LFIELD)), IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABOPN (TMPTAB, 'READ', IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, NCLNG(LFIELD),
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABCLO (TMPTAB, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABDES (TMPTAB, IERR)
               IF (IERR.NE.0) GO TO 990
 60            CONTINUE
            END IF
         END IF
      MSGSUP = MSGSAV
C                                       Call arguments
C                                       FT type
      IF (CMET.EQ.'DFT ') THEN
         METHOD = -1
      ELSE IF (CMET.EQ.'GRID') THEN
         METHOD = 1
      ELSE
         METHOD = 0
         END IF
C                                       Model type

      IF (CMOD.EQ.'IMAG') THEN
         MODEL = 2
C                                       Can't DFT an image
         METHOD = 1
      ELSE
         MODEL = 1
         END IF
      CHANEL = CHANL
      MCHAN = NCHAN
      JBUFSZ = BUFSIZ * 2
      DOSUM = .FALSE.
      FREQID = -1
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OUVCGT (UVIN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBCLOS (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       3D imaging ?
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETDO3 (DISKI, CNOI, OBUFFR(1,BUFNO3), IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Subtract
      CALL APOBJ ('OPEN', 'OUMSUB', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL UVMSUB (DISKI, CNOI, DISKO, CNOO, IFIELD, MODEL, METHOD,
     *   CHANEL, MCHAN, DOSUM, DOMSG, CATBLK, JBUFSZ, FREQID,
     *   OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), OBUFFR(1,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('FREE', 'OUMSUB', LERR)
      APOPEN = .FALSE.
C                                       Save numbers of components
C                                       subtracted if appropriate.
      IF (CMOD.EQ.'CC  ') THEN
         DIM(1) = NCCB
         DIM(2) = 1
         CALL OUVPUT (UVIN, 'MODCCBEG', OOAINT, DIM, NSUBG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset VISOFF
      COUNT = 0
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary objects
      CALL OBFREE (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBFREE (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Reset UVIN selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       If created output save info
      IF (NEWOUT) THEN
         CALL OBSCNF (UVOUT, CCNO, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUMSUB', LERR)
         END IF
      MSGTXT = 'OUMSUB: ERROR SUBTRACTING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'OUMSUB: FROM ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUMDIV (UVIN, UVOUT, NFIELD, IMAGE, CHANL, NCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Divide the Fourier transform of an image model into a uvdata set.
C   Can use either a set of clean components or an image.
C      Note: if the FT of an image is to be used then it should be the
C   first or only plane in the image(s).
C      Note: selection by IF, channel and Stokes is disabled in UVIN
C   except for CHANL and NCHAN
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      NFIELD  I    Number of fields
C      IMAGE   C(*)*? array of image names to divide.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R(2)  Model, data factors (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model flux in Jy, (0.0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C      MODPARMS  R(4)  Point model: extended object parameters
C                       1=> model type, 0=point, 1=gaussian, 2,3=sphere
C                       Gaussian: (2)=major axis(asec), (3)=minor axis
C                                 (4)=PA (degrees)
C                       Sphere: (2)=radius (asec).
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, BUFNO3, LERR,
     *   MSGSAV, SCRNO, TYPE, DIM(7), MODEL, METHOD, CHANEL, MCHAN,
     *   JBUFSZ, I, NS, FREQID, COUNT, BCHAN, ECHAN, BIF, EIF, DUMMY
      LOGICAL   DOSUM, DOMSG, GETNCC, NEWOUT, INOUT, APOPEN
      CHARACTER TEMPUV*32, TEMP2*32, TMPTAB*32, CMOD*4, CMET*4,
     *   STOKES*4, SORD*2, CDUMMY*1, UVTYPE*2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSCD.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      COMPDT = .FALSE.
      DATDIV = .TRUE.
      APOPEN = .FALSE.
      IERR = 0
C                                       Is input = output?
      INOUT = UVIN .EQ. UVOUT
      MSGSAV = MSGSUP
C                                       Disk, CNO
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save and reset selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output may not exist
      MSGSUP = 32000
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DISKO = 0
         CNOO = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       DFIL.INC scratch file number.
      SCRNO = 0
      CALL OUVGET (UVIN, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKI = 0
         CNOI = SCRNO
C                                       Clear any flags
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (UVIN, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
         IERR = 0
         CALL OUCCLR (UVIN, IERR)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Output
      SCRNO = 0
      MSGSUP = 32000
      CALL OUVGET (UVOUT, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKO = 0
         CNOO = SCRNO
      END IF
      IF (IERR.NE.0) GO TO 995
C                                       Does output exist
      NEWOUT = (DISKO.LE.0) .AND. (CNOO.LE.0)
C                                       Make temporary objects for
C                                       buffer
      TEMPUV = 'Temporary uv data 1 for OUMDIV'
      CALL OBCREA (TEMPUV, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         TEMP2 = 'Temporary uv data 2 for OUMDIV'
         CALL OBCREA (TEMP2, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open for buffers, update info
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'OUMDIV DOES NOT WORK FOR UV DATA OF TYPE ''' //
     *      UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBOPEN (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Save vis count.
         CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       This funny business to fool the
C                                       I/O which thinks no data will be
C                                       written.
         CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVOUT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get buffer numbers
      CALL OBINFO (UVIN, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPUV, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBINFO (TEMP2, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OBINFO (UVOUT, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Set values in common
      MFIELD = NFIELD
      MSGSUP = 32000
C                                       CC version default = highest
      CALL OUVGET (UVIN, 'MODCCVER', TYPE, DIM, CCVER, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (MAXFLD, 1, CCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCBEG', TYPE, DIM, NSUBG, CDUMMY, IERR)
      NS = DIM(1)
      IF (IERR.EQ.1) THEN
         NS = MAXFLD
         IERR = 0
         CALL FILL (NS, 1, NSUBG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Highest component
      CALL OUVGET (UVIN, 'MODCCEND', TYPE, DIM, NCLNG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NS = MAXFLD
         CALL FILL (NS, 0, NCLNG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Defaulted no. components?
      GETNCC = .TRUE.
      DO 80 I = 1,NFIELD
         IF (NCLNG(I).GT.0) GETNCC = .FALSE.
 80      CONTINUE
C                                       Do negative?
      CALL OUVGET (UVIN, 'MODNONEG', TYPE, DIM, NONEG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NONEG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Flux cutoff
      CALL OUVGET (UVIN, 'MODFLUX', TYPE, DIM, LIMFLX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         LIMFLX = -1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model type
      CALL OUVGET (UVIN, 'MODMODEL', TYPE, DIM, DUMMY, CMOD, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMOD = 'CC  '
         END IF
      IF (IERR.NE.0) GO TO 995
      GETNCC = GETNCC .AND. (CMOD.EQ.'CC  ')
C                                       Model method
      CALL OUVGET (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMET = 'DFT '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model Factor
      CALL OUVGET (UVIN, 'MODFACT ', TYPE, DIM, FACGRD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FACGRD(1) = 1.0
         FACGRD(2) = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Progress report?
      CALL OUVGET (UVIN, 'MODDOMSG', TYPE, DIM, DOMSG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOMSG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Point model
      CALL OUVGET (UVIN, 'MODDOPT ', TYPE, DIM, DOPTMD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOPTMD = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (DOPTMD) THEN
C                                       Flux
         CALL OUVGET (UVIN, 'MODPTFLX ', TYPE, DIM, PTFLX, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTFLX = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X offset
         CALL OUVGET (UVIN, 'MODPTXOF ', TYPE, DIM, PTRAOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTRAOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y offset
         CALL OUVGET (UVIN, 'MODPTYOF ', TYPE, DIM, PTDCOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTDCOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       extended object parameters
         CALL OUVGET (UVIN, 'MODPARMS ', TYPE, DIM, PARMOD, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PARMOD(1) = 0.0
            PARMOD(2) = 0.0
            PARMOD(3) = 0.0
            PARMOD(4) = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Use DFT for point model.
         CMET = 'DFT '
C                                       Disk numbers, no. CC
      ELSE
C                                       Loop over fields
         MSGSUP = MSGSAV
         TMPTAB = 'Temporary table for OUMDIV'
         DO 200 I = 1,NFIELD
            CALL OBDSKC (IMAGE(I),  CCDISK(I), CCCNO(I), IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Number of components
            IF (GETNCC) THEN
               CALL IM2TAB (IMAGE(I), TMPTAB, 'CC', ABS(CCVER(I)), IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABOPN (TMPTAB, 'READ', IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, NCLNG(I),
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABCLO (TMPTAB, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TABDES (TMPTAB, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
 200        CONTINUE
         END IF
      MSGSUP = MSGSAV
C                                       Call arguments
C                                       FT type
      IF (CMET.EQ.'DFT ') THEN
         METHOD = -1
      ELSE IF (CMET.EQ.'GRID') THEN
         METHOD = 1
      ELSE
         METHOD = 0
         END IF
C                                       Model type

      IF (CMOD.EQ.'IMAG') THEN
         MODEL = 2
C                                       Can't DFT an image
         METHOD = 1
      ELSE
         MODEL = 1
         END IF
      CHANEL = CHANL
      MCHAN = NCHAN
      JBUFSZ = BUFSIZ * 2
      DOSUM = .FALSE.
      FREQID = -1
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OUVCGT (UVIN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBCLOS (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       3D imaging ?
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETDO3 (DISKI, CNOI, OBUFFR(1,BUFNO3), IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Divide
      CALL APOBJ ('OPEN', 'OUMDIV', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL UVMDIV (DISKI, CNOI, DISKO, CNOO, MODEL, METHOD, DOMSG,
     *   CHANEL, MCHAN, CATBLK, JBUFSZ, FREQID, OBUFFR(1,BUFNO1),
     *   OBUFFR(1,BUFNO2), OBUFFR(1,BUFNO3), OBUFFR(1024,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('FREE', 'OUMDIV', LERR)
      APOPEN = .FALSE.
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset VISOFF
      COUNT = 0
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary objects
      CALL OBFREE (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBFREE (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Reset UVIN selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       If created output save info
      IF (NEWOUT) CALL OBSCNF (UVOUT, CCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUMDIV', LERR)
         END IF
      MSGTXT = 'OUMDIV: ERROR DIVIDING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'OUMDIV: INTO ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUPDIV (UVIN, UVOUT, NFIELD, PIMAGE, CHANL, NCHAN,
     *   IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Divide the Fourier transform of an polarized image model into a
C   uvdata set.  Only RL and LR portions of output uv data are valid.
C   Can use either a set of clean components or an image.
C      Note: if the FT of an image is to be used then it should be the
C   first or only plane in the image(s).
C      Note: selection by IF, channel and Stokes is disabled in UVIN
C   except for CHANL and NCHAN
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      NFIELD  I    Number of fields
C      PIMAGE  C(2,*)*? array of image names to divide as Q, U
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODPCVER  I(2,*)  CC version number for each image (1, i.e.
C                        must specify for line data.) per Q, U
C      MODPCEND  I(2,*)  NUmber of components to use per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R(2)  Model, data factors (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model (i,q,u,v) flux in Jy, (0,0,0,0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), PIMAGE(2,NFIELD)*(*)
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, BUFNO3, LERR,
     *   MSGSAV, SCRNO, TYPE, DIM(7), MODEL, METHOD, CHANEL, MCHAN,
     *   JBUFSZ, I, IP, NS, FREQID, COUNT, BCHAN, ECHAN, BIF, EIF,
     *   DUMMY, CAT(256)
      LOGICAL   DOSUM, DOMSG, GETNCC, NEWOUT, INOUT, APOPEN
      CHARACTER TEMPUV*32, TEMP2*32, TMPTAB*32, CMOD*4, CMET*4,
     *   STOKES*4, SORD*2, CDUMMY*1, UVTYPE*2
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   PCCDSK(2,MAXFLD), PCCNO(2,MAXFLD), PCVER(2,MAXFLD),
     *   NPCC(2,MAXFLD)
      REAL      PFLUX(4)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSCD.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      COMPDT = .FALSE.
      DATDIV = .TRUE.
      APOPEN = .FALSE.
      IERR = 0
C                                       Is input = output?
      INOUT = UVIN .EQ. UVOUT
      MSGSAV = MSGSUP
C                                       Disk, CNO
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save and reset selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output may not exist
      MSGSUP = 32000
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DISKO = 0
         CNOO = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       DFIL.INC scratch file number.
      SCRNO = 0
      CALL OUVGET (UVIN, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKI = 0
         CNOI = SCRNO
C                                       Clear any flags
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (UVIN, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
         IERR = 0
         CALL OUCCLR (UVIN, IERR)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Output
      SCRNO = 0
      MSGSUP = 32000
      CALL OUVGET (UVOUT, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKO = 0
         CNOO = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Does output exist
      NEWOUT = (DISKO.LE.0) .AND. (CNOO.LE.0)
C                                       Make temporary objects for
C                                       buffer
      MSGSUP = MSGSAV
      TEMPUV = 'Temporary uv data 1 for OUPDIV'
      CALL OBCREA (TEMPUV, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         TEMP2 = 'Temporary uv data 2 for OUPDIV'
         CALL OBCREA (TEMP2, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open for buffers, update info
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'OUPDIV DOES NOT WORK FOR UV DATA OF TYPE ''' //
     *      UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBOPEN (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Save vis count.
         CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       This funny business to fool the
C                                       I/O which thinks no data will be
C                                       written.
         CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVOUT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get buffer numbers
      CALL OBINFO (UVIN, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPUV, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBINFO (TEMP2, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OBINFO (UVOUT, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Set values in common
      MFIELD = NFIELD
      MSGSUP = 32000
C                                       CC version default = 1
      CALL OUVGET (UVIN, 'MODPCVER', TYPE, DIM, PCVER, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (MAXFLD*2, 1, PCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Highest component
      CALL OUVGET (UVIN, 'MODPCEND', TYPE, DIM, NPCC, CDUMMY, IERR)
      GETNCC = .FALSE.
      IF (IERR.EQ.1) THEN
         IERR = 0
         GETNCC = .TRUE.
         NS = MAXFLD
         CALL FILL (NS*2, 0, NPCC)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Defaulted no. components?
      COUNT = 0
      DO 80 I = 1,DIM(2)
         COUNT = COUNT + NPCC(1,I) + NPCC(2,I)
 80      CONTINUE
         IF (COUNT.LE.0) GETNCC = .TRUE.
C                                       Do negative?
      CALL OUVGET (UVIN, 'MODNONEG', TYPE, DIM, NONEG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NONEG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Flux cutoff
      CALL OUVGET (UVIN, 'MODFLUX', TYPE, DIM, LIMFLX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         LIMFLX = -1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model type
      CALL OUVGET (UVIN, 'MODMODEL', TYPE, DIM, DUMMY, CMOD, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMOD = 'CC  '
         END IF
      IF (IERR.NE.0) GO TO 995
      GETNCC = GETNCC .AND. (CMOD.EQ.'CC  ')
C                                       Model method
      CALL OUVGET (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMET = 'DFT '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model Factor
      CALL OUVGET (UVIN, 'MODFACT ', TYPE, DIM, FACGRD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FACGRD(1) = 1.0
         FACGRD(2) = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Progress report?
      CALL OUVGET (UVIN, 'MODDOMSG', TYPE, DIM, DOMSG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOMSG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Point model
      CALL OUVGET (UVIN, 'MODDOPT ', TYPE, DIM, DOPTMD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOPTMD = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (DOPTMD) THEN
C                                       Flux
         CALL OUVGET (UVIN, 'MODPTFLX ', TYPE, DIM, PFLUX, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL RFILL (4, 0.0, PFLUX)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X offset
         CALL OUVGET (UVIN, 'MODPTXOF ', TYPE, DIM, PTRAOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTRAOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y offset
         CALL OUVGET (UVIN, 'MODPTYOF ', TYPE, DIM, PTDCOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTDCOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Use DFT for point model.
         CMET = 'DFT '
C                                       Disk numbers, no. CC
C                                       Loop over fields
      ELSE
         MSGSUP = MSGSAV
         TMPTAB = 'Temporary table for OUPDIV'
         DO 200 I = 1,NFIELD
            DO 190 IP = 1,2
               CALL OBDSKC (PIMAGE(IP,I),  PCCDSK(IP,I), PCCNO(IP,I),
     *            IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Number of components
               IF (GETNCC) THEN
                  CALL IM2TAB (PIMAGE(IP,I), TMPTAB, 'CC', PCVER(IP,I),
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL TABOPN (TMPTAB, 'READ', IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, NPCC(IP,I),
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL TABCLO (TMPTAB, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL TABDES (TMPTAB, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
 190           CONTINUE
            CCDISK(I) = PCCDSK(1,I)
            CCCNO(I) = PCCNO(1,I)
 200        CONTINUE
         END IF
      MSGSUP = MSGSAV
C                                       Call arguments
C                                       FT type
      IF (CMET.EQ.'DFT ') THEN
         METHOD = -1
      ELSE IF (CMET.EQ.'GRID') THEN
         METHOD = 1
      ELSE
         METHOD = 0
         END IF
C                                       Model type
      IF (CMOD.EQ.'IMAG') THEN
         MODEL = 2
C                                       Can't DFT an image
         METHOD = 1
      ELSE
         MODEL = 1
         END IF
      CHANEL = CHANL
      MCHAN = NCHAN
      JBUFSZ = BUFSIZ * 2
      DOSUM = .FALSE.
      FREQID = -1
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OUVCGT (UVIN, CAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL COPY (256, CAT, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBCLOS (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       3D imaging ?
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETDO3 (DISKI, CNOI, OBUFFR(1,BUFNO3), IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Divide
      CALL APOBJ ('OPEN', 'OUPDIV', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL UVPDIV (DISKI, CNOI, DISKO, CNOO, MODEL, METHOD, DOMSG,
     *   CHANEL, MCHAN, CAT, JBUFSZ, FREQID,
     *   NFIELD, PCCDSK, PCCNO, PCVER, NPCC, PFLUX,
     *   OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), OBUFFR(1,BUFNO3),
     *   OBUFFR(1024,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('FREE', 'OUPDIV', LERR)
      APOPEN = .FALSE.
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset VISOFF
      COUNT = 0
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary objects
      CALL OBFREE (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBFREE (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Reset UVIN selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       If created output save info
      IF (NEWOUT) CALL OBSCNF (UVOUT, CCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUPDIV', LERR)
         END IF
      MSGTXT = 'OUPDIV: ERROR DIVIDING ' // PIMAGE(1,1)
      CALL MSGWRT (8)
      MSGTXT = 'OUPDIV: INTO ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUCDFT (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL,
     *   NCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Add the DFT Fourier transform of a "W" corrected image model to a uv
C   data set.   Only point models are processed.
C   Can also optionally make minor corrections for bandwidth smearing
C   and correct the values of u,v, and w for errors in the reference
C   frequency.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      NFIELD  I    Number of fields. (Only 1 supported)
C      IMAGE   C(*)*? array of image names to subtract.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODFACT   R(2)  Model, data factors, use -1.0 to subtract (1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MAXBWC    R     Maximum bandwidth smearing correction in % (0)
C      BWCOR     R     Bandwidth correction factor. 0 => no corr. (0)
C                      Multiplied times nominal (CATBLK) bandwidth.
C      FRQCOR    R     Ref. frequency correction factor. 0 => no corr.
C                      (0)
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, BUFNO3, NS,
     *   MSGSAV, SCRNO, TYPE, DIM(7), CHANEL, MCHAN, JBUFSZ, I, FREQID,
     *   COUNT, BIF, EIF, BCHAN, ECHAN, NCCB, DUMMY, LERR, LFIELD, LF1,
     *   LF2
      LOGICAL   DOSUM, DOMSG, GETNCC, NEWOUT, INOUT, APOPEN
      REAL      MAXBWC, BWCOR, FRQCOR
      CHARACTER TEMPUV*32, TEMP2*32, TMPTAB*32, STOKES*4, SORD*2,
     *   CDUMMY*1, UVTYPE*2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      APOPEN = .FALSE.
C                                       Is input = output?
      INOUT = UVIN .EQ. UVOUT
      MSGSAV = MSGSUP
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = NFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
C                                       Disk, CNO
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save and reset selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output may not exist
      MSGSUP = 32000
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DISKO = 0
         CNOO = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       DFIL.INC scratch file number.
      SCRNO = 0
      CALL OUVGET (UVIN, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKI = 0
         CNOI = SCRNO
C                                       Clear any flags
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (UVIN, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
         IERR = 0
         CALL OUCCLR (UVIN, IERR)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Output
      SCRNO = 0
      MSGSUP = 32000
      CALL OUVGET (UVOUT, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKO = 0
         CNOO = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Does output exist
      NEWOUT = (DISKO.LE.0) .AND. (CNOO.LE.0)
C                                       Make temporary object for buffer
      MSGSUP = MSGSAV
      TEMPUV = 'Temporary uv data 1 for OUCDFT'
      CALL OBCREA (TEMPUV, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         TEMP2 = 'Temporary uv data 2 for OUCDFT'
         CALL OBCREA (TEMP2, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open for buffers, update info
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'OUCDFT DOES NOT WORK FOR UV DATA OF TYPE ''' //
     *      UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBOPEN (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Save vis count.
         CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       This funny business to fool the
C                                       I/O which thinks no data will be
C                                       written.
         CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVOUT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get buffer numbers
      CALL OBINFO (UVIN, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPUV, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBINFO (TEMP2, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OBINFO (UVOUT, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Set values in common
      MFIELD = NFIELD
      MSGSUP = 32000
C                                       CC version default = 1
      CALL OUVGET (UVIN, 'MODCCVER', TYPE, DIM, CCVER, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (MAXFLD, 1, CCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCBEG', TYPE, DIM, NSUBG, CDUMMY, IERR)
      NCCB = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCB = MAXFLD
         IERR = 0
         CALL FILL (NCCB, 1, NSUBG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Highest component
      CALL OUVGET (UVIN, 'MODCCEND', TYPE, DIM, NCLNG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NS = MAXFLD
         CALL FILL (NS, 0, NCLNG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Defaulted no. components?
      GETNCC = .TRUE.
      DO 20 I = 1,DIM(1)
         IF (NCLNG(I).GT.0) GETNCC = .FALSE.
 20      CONTINUE
C                                       Do negative?
      CALL OUVGET (UVIN, 'MODNONEG', TYPE, DIM, NONEG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NONEG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Flux cutoff
      CALL OUVGET (UVIN, 'MODFLUX', TYPE, DIM, LIMFLX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         LIMFLX = -1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model Factor
      CALL OUVGET (UVIN, 'MODFACT ', TYPE, DIM, FACGRD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FACGRD(1) = 1.0
         FACGRD(2) = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Progress report?
      CALL OUVGET (UVIN, 'MODDOMSG', TYPE, DIM, DOMSG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOMSG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Max. bw smearing correction
      CALL OUVGET (UVIN, 'MAXBWC', TYPE, DIM, MAXBWC, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         MAXBWC = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Band width correction factor
      CALL OUVGET (UVIN, 'BWCOR', TYPE, DIM, BWCOR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         BWCOR = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       FRQCOR
      CALL OUVGET (UVIN, 'FRQCOR', TYPE, DIM, FRQCOR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FRQCOR = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Disk numbers, no. CC
C                                       Loop over fields
      MSGSUP = MSGSAV
C                                       3D imaging requires all set
      DO 50 LFIELD = 1,NFIELD
         CALL OBDSKC (IMAGE(LFIELD),  CCDISK(LFIELD), CCCNO(LFIELD),
     *      IERR)
         IF (IERR.NE.0) GO TO 990
 50      CONTINUE
      IF (GETNCC) THEN
         TMPTAB = 'Temporary table for OUCDFT'
C                                       Number of components
         DO 60 LFIELD = 1,NFIELD
            CALL IM2TAB (IMAGE(LFIELD), TMPTAB, 'CC',
     *         ABS(CCVER(LFIELD)), IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABOPN (TMPTAB, 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, NCLNG(LFIELD),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABCLO (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABDES (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
 60         CONTINUE
         END IF
      CHANEL = CHANL
      MCHAN = NCHAN
      JBUFSZ = BUFSIZ * 2
      DOSUM = .FALSE.
      NGRDAT = .FALSE.
      FREQID = -1
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OUVCGT (UVIN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBCLOS (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       3D imaging ?
      CALL SETDO3 (DISKI, CNOI, OBUFFR(1,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Add
      CALL APOBJ ('OPEN', 'OUCDFT', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL VSCDFT (CHANEL, MCHAN, DISKI, CNOI, DISKO, CNOO, IFIELD,
     *   DOSUM, DOMSG, MAXBWC, BWCOR, FRQCOR, CATBLK, JBUFSZ,
     *   OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), OBUFFR(1,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('FREE', 'OUCDFT', LERR)
      APOPEN = .FALSE.
C                                       Save numbers of components
C                                       added.
      DIM(1) = NCCB
      DIM(2) = 1
      CALL OUVPUT (UVIN, 'MODCCBEG', OOAINT, DIM, NSUBG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset VISOFF
      COUNT = 0
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary objects
      CALL OBFREE (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBFREE (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Reset UVIN selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       If created output save info
      IF (NEWOUT) CALL OBSCNF (UVOUT, CCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUCDFT', LERR)
         END IF
      MSGTXT = 'OUCDFT: ERROR ADDING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'OUCDFT: TO ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUFDFT (UVIN, UVOUT, NFIELD, IMAGE, CHANL, NCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Add the DFT Fourier transform of a bandwidth smearing corrected
C   image model to a uv  data set.  Only point models are processed.
C   Can also optionally correct the values of u,v, and w for errors in
C   the reference frequency.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      NFIELD  I    Number of fields. (Only 1 supported)
C      IMAGE   C(*)*? array of image names to subtract.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODFACT   R(2)  Model, data factors, use -1.0 to subtract (1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MAXBWC    R     Maximum bandwidth smearing correction in % (200)
C      BWCOR     R     Bandwidth correction factor. 0 => no corr. (0)
C                      Multiplied times nominal (CATBLK) bandwidth.
C      FRQCOR    R     Ref. frequency correction factor. 0 => no corr.
C                      (0)
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, BUFNO3, LERR,
     *   MSGSAV, SCRNO, TYPE, DIM(7), CHANEL, MCHAN, DUMMY, JBUFSZ, I,
     *   NS, FREQID, COUNT, BIF, EIF, BCHAN, ECHAN, NCCS, LFIELD
      LOGICAL   DOSUM, DOMSG, GETNCC, NEWOUT, INOUT, APOPEN
      REAL      MAXBWC, BWCOR, FRQCOR
      CHARACTER TEMPUV*32, TEMP2*32, TMPTAB*32, STOKES*4, SORD*2,
     *   CDUMMY*1, UVTYPE*2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      APOPEN = .FALSE.
C                                       Is input = output?
      INOUT = UVIN .EQ. UVOUT
      MSGSAV = MSGSUP
C                                       Disk, CNO
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save and reset selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output may not exist
      MSGSUP = 32000
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DISKO = 0
         CNOO = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       DFIL.INC scratch file number.
      SCRNO = 0
      CALL OUVGET (UVIN, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKI = 0
         CNOI = SCRNO
C                                       Clear any flags
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (UVIN, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
         IERR = 0
         CALL OUCCLR (UVIN, IERR)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Output
      SCRNO = 0
      MSGSUP = 32000
      CALL OUVGET (UVOUT, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKO = 0
         CNOO = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Does output exist
      NEWOUT = (DISKO.LE.0) .AND. (CNOO.LE.0)
C                                       Make temp. objects for buffer
      MSGSUP = MSGSAV
      TEMPUV = 'Temporary uv data 1 for OUFDFT'
      CALL OBCREA (TEMPUV, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         TEMP2 = 'Temporary uv data 2 for OUFDFT'
         CALL OBCREA (TEMP2, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open for buffers, update info
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'OUFDFT DOES NOT WORK FOR UV DATA OF TYPE ''' //
     *      UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBOPEN (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Save vis count.
         CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       This funny business to fool the
C                                       I/O which thinks no data will be
C                                       written.
         CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVOUT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get buffer numbers
      CALL OBINFO (UVIN, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (TEMPUV, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBINFO (TEMP2, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OBINFO (UVOUT, BUFNO3, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Set values in common
      MFIELD = NFIELD
      MSGSUP = 32000
C                                       CC version default = 1
      CALL OUVGET (UVIN, 'MODCCVER', TYPE, DIM, CCVER, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (MAXFLD, 1, CCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCBEG', TYPE, DIM, NSUBG, CDUMMY, IERR)
      NCCS = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCS = MAXFLD
         IERR = 0
         CALL FILL (NCCS, 1, NSUBG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Highest component
      CALL OUVGET (UVIN, 'MODCCEND', TYPE, DIM, NCLNG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NS = MAXFLD
         CALL FILL (NS, 0, NCLNG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Defaulted no. components?
      GETNCC = .TRUE.
      DO 80 I = 1,DIM(1)
         IF (NCLNG(I).GT.0) GETNCC = .FALSE.
 80      CONTINUE
C                                       Do negative?
      CALL OUVGET (UVIN, 'MODNONEG', TYPE, DIM, NONEG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NONEG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Flux cutoff
      CALL OUVGET (UVIN, 'MODFLUX', TYPE, DIM, LIMFLX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         LIMFLX = -1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model Factor
      CALL OUVGET (UVIN, 'MODFACT ', TYPE, DIM, FACGRD, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FACGRD(1) = 1.0
         FACGRD(2) = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Progress report?
      CALL OUVGET (UVIN, 'MODDOMSG', TYPE, DIM, DOMSG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOMSG = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Max. bw smearing correction
      CALL OUVGET (UVIN, 'MAXBWC', TYPE, DIM, MAXBWC, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         MAXBWC = 200.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Band width correction factor
      CALL OUVGET (UVIN, 'BWCOR', TYPE, DIM, BWCOR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         BWCOR = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       FRQCOR
      CALL OUVGET (UVIN, 'FRQCOR', TYPE, DIM, FRQCOR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FRQCOR = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Disk numbers, no. CC
C                                       Loop over fields
      MSGSUP = MSGSAV
      TMPTAB = 'Temporary table for OUFDFT'
      DO 200 LFIELD = 1,NFIELD
         CALL OBDSKC (IMAGE(LFIELD),  CCDISK(LFIELD), CCCNO(LFIELD),
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Number of components
         IF (GETNCC) THEN
            CALL IM2TAB (IMAGE(LFIELD), TMPTAB, 'CC',
     *         ABS(CCVER(LFIELD)), IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABOPN (TMPTAB, 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, NCLNG(LFIELD),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABCLO (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABDES (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 200     CONTINUE
      CHANEL = CHANL
      MCHAN = NCHAN
      JBUFSZ = BUFSIZ * 2
      DOSUM = .FALSE.
      FREQID = -1
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OUVCGT (UVIN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBCLOS (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       3D imaging ?
      CALL SETDO3 (DISKI, CNOI, OBUFFR(1,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Add
      CALL APOBJ ('OPEN', 'OUFDFT', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL VSFDFT (CHANEL, MCHAN, DISKI, CNOI, DISKO, CNOO, 0,
     *   DOSUM, DOMSG, MAXBWC, BWCOR, FRQCOR, CATBLK, JBUFSZ,
     *   OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), OBUFFR(1,BUFNO3), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('FREE', 'OUFDFT', LERR)
      APOPEN = .FALSE.
C                                       Save numbers of components
C                                       added.
      DIM(1) = NCCS
      DIM(2) = 1
      CALL OUVPUT (UVIN, 'MODCCBEG', OOAINT, DIM, NSUBG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset VISOFF
      COUNT = 0
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary objects
      CALL OBFREE (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBFREE (TEMP2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Reset UVIN selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       If created output save info
      IF (NEWOUT) CALL OBSCNF (UVOUT, CCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUFDFT', LERR)
         END IF
      MSGTXT = 'OUFDFT: ERROR ADDING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'OUFDFT: TO ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUVIMG (UVDATA, IFIELD, NFIELD, IMAGE, BEAM, WORK1,
     *   WORK2, DOBEAM, CHAN, NCHAN, IMCHAN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Makes 1 beam or 1 image from a uv data set.  If NFIELD is <= 0 then
C   a beam is made.  If an image is to be made the normalization factor
C   is obtained from the beam, if absent, the beam is remade.
C   The input uvdata is assumed to have been calibrated, selected and
C   had any uniform weighting corrections applied.
C   Note: output IMAGE and BEAM objects should exist (full
C   instantation) prior to call but WORK1 and WORK2 may be created.
C      Two methods of Fourier transform are available: FFT and DFT.  The
C   FFT method supports multiple fields and allows images to be a
C   different size from the beam.  The DFT method does a full 3D DFT but
C   supports only a single field and the beam must be the same size as
C   the image.
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      IFIELD  I(*) Field to image: 0 => all NFIELD (note 1 beam made on
C                   IFIELD = 1 when DOBEAM true and DO3DIM false)
C                   Can be array of fields to image - give the
C                   field#+1000000 to indicate this - first
C                   value < 1000000  or 1000 fields ends list.
C      NFIELD  I    Number of fields.
C      IMAGE   C(*)*? array of image names
C      BEAM    C(*)*? Array of beam names
C      WORK1   C*?  Scratch image large enough for FT of largest field
C                   (used for FFT imaging only)
C      WORK2   C*?  Scratch image large enough for FT of largest field
C                   (used for FFT imaging only)
C      DOBEAM  L    If true make a beam else make an image
C      CHAN    I    First channel in uv data to image
C      NCHAN   I    Number of channels to "average" into the image
C      IMCHAN  I    First channel number in output image or beam
C   Inputs attached to UVDATA:
C      STOKES    C*4   Desired Stokes parameter (I)
C      UVRANGE   R(2)  UV range in kilo wavelengths (all)
C      GUARDBND  R(2)  Fractional guardband areound edge of uv grid (0)
C   Inputs attached to IMAGE(1): (defaulted where approproate)
C      FTTYPE    C*4   Fourier transform type 'FFT' or 'DFT'. ('FFT')
C      IMSIZE    I(2,*) Image size per field (no default)
C      CELLSIZE  R(2)  Cellsize in arcseconds in X and Y (no default)
C      CHTYPE    C*4   'LINE',  or 'SUM ' for imaging ('SUM')
C      SHIFT     R(2)  Shift in arcsec (DFT imaging)
C      RASHIFT   R(*)  X position shift in arcseconds per field (0) FFT
C      DECSHIFT  R(*)  Y position shift in arcseconds per field (0) FFT
C      CENTERX   I(*)  Center X pixel position per field (std default)
C      CENTERY   I(*)  Center Y pixel position per field (std default)
C      CTYPX     I     X convolving function type (std default)
C      XPARM     R(10) X convolving function parameters( std default)
C      CTYPY     I     Y convolving function type (std default)
C      YPARM     R(10) Y convolving function parameters (std default)
C      DOZERO    L     IF true do Zero spacing flux (do if value given)
C      ZEROSP    R(5)  Zero spacing parameters (no zero spacing flux)
C      TFLUXG    R     Total flux to be subtracted from ZEROSP (0.0)
C      DOTAPER   L     If true taper (do if non zero taper given)
C      UVTAPER   R(2)  X and Y taper values (no taper)
C   Inputs attached to BEAM:
C      IMSIZE    I(2)  Size of beam (no default)
C      SUMWTS    R     Sum of weights used for normalization (make beam)
C                      Set when beam gridded.
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      INTEGER   IFIELD(*), NFIELD, CHAN, NCHAN, IMCHAN, IERR
      LOGICAL   DOBEAM
      CHARACTER UVDATA*(*), IMAGE(*)*(*), BEAM(*)*(*), WORK1*(*),
     *   WORK2*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   MXIMAG
      PARAMETER (MXIMAG=1000)
      CHARACTER FTTYPE*4, CDUMMY*1, SORD*2, UVTYPE*2, CNAME*8, KEYW*8
      INTEGER   LFIELD, DISKI, CNOI, DISKO(MAXFLD), CNOO(MAXFLD), I,
     *   SCRGRD, SCRWRK, JBUFSZ, BUFNO, TYPE, DIM(7), MSGSAV, SCRNO,
     *   NAXIS(7), NUCHOU, BMSIZ(2), NEED, APSIZE, LREC, NX, NY, CHINC,
     *   DUMMY, LLREC, LERR, BFIELD, LFLD1, LFLD2, LOOP, NLOOP, I1, I2,
     *   I3, FIELDS(MXIMAG), BNEED, LNEED, JFLD1, JFLD2, NCHOIC,
     *   NUMRES, NFPRES
      LONGINT   TNEED
      LOGICAL   DOCREA, DOINIT, DOSEL, DOGCOR, ISZERO, DOBM, MAKWRK,
     *   MAKGRD, FAST, APOPEN, WDOBM, ALL, CHOSEN
      REAL      SUMWTS(MAXCIF), CAT2R(256), CATUVR(256), CATR(256),
     *   MAXUU, SWTS, UVTAPR(2), TAPERS(2,MXIMAG), XTREMA(2,MXIMAG),
     *   BEAMS(3,MXIMAG)
      EQUIVALENCE (CATBLK, CATR)
      SAVE SUMWTS
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DAPM.INC'
      SAVE DOINIT
      DATA DOINIT /.TRUE./
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      MSGSAV = MSGSUP
      APOPEN = .FALSE.
      MSGSUP = 32000
      CALL IMGET (IMAGE(1), 'NUMRES', TYPE, DIM, NUMRES, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         NUMRES = 1
         END IF
      IF (IERR.NE.0) GO TO 995
      NUMRES = MAX (1, NUMRES)
      NFPRES = NFIELD / NUMRES
      MSGSUP = 32000
      CALL IMGET (IMAGE(1), 'DO3DIMAG', TYPE, DIM, DO3DIM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         DO3DIM = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSUP = 32000
      CALL IMGET (IMAGE(1), 'ONEBEAM', TYPE, DIM, ONEBEM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         ONEBEM = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      CHOSEN = .FALSE.
      NCHOIC = 0
      IF (IFIELD(1).EQ.0) THEN
         LFLD1 = 1
         LFLD2 = NFIELD
      ELSE IF (IFIELD(1).GT.1000000) THEN
         CHOSEN = .TRUE.
         LFLD1 = NFIELD
         LFLD2 = 1
         DO 5 I = 1,MXIMAG
            IF (IFIELD(I).GT.1000000) THEN
               NCHOIC = NCHOIC + 1
               LFIELD = IFIELD(I) - 1000000
               LFLD1 = MIN (LFLD1, LFIELD)
               LFLD2 = MAX (LFLD2, LFIELD)
            ELSE
               GO TO 6
               END IF
 5          CONTINUE
      ELSE
         LFLD1 = ABS (IFIELD(1))
         LFLD2 = LFLD1
         IF (LFLD1.GT.NFIELD) GO TO 999
         END IF
C                                       Sum weight as header keyword
 6    CNAME = 'IMAGE'
      KEYW = 'SWEIGHTS'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       DFT or FFT
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         CHINC = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      CHINC = MAX (1, CHINC)
C                                       DFT or FFT
      CALL IMGET (IMAGE(1), 'FTTYPE', TYPE, DIM, DUMMY, FTTYPE, IERR)
      MSGSUP = MSGSAV
C                                      Default = FFT
      IF (IERR.EQ.1) THEN
         FTTYPE = 'FFT '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Branch by FT type
      NEED = 0
      TNEED = 0
      IF (FTTYPE.EQ.'FFT ') THEN
         MFIELD = NFIELD
         JBUFSZ = BUFSIZ * 2
         DOGCOR = .TRUE.
         DOSEL = .FALSE.
         DOUNIF = .FALSE.
         DOCREA = .FALSE.
         WTPOWR = 1.0
C                                       Disk numbers of images/beam
         IF (DOBEAM) THEN
            DO 10 LFIELD = LFLD1,LFLD2
               BFIELD = MAX (1, LFIELD)
               IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
               CALL OBDSKC (BEAM(BFIELD), DISKO(BFIELD), CNOO(BFIELD),
     *            IERR)
               BEMVOL(LFIELD) = DISKO(BFIELD)
               CNOBEM(LFIELD) = CNOO(BFIELD)
               IF (IERR.NE.0) GO TO 990
 10            CONTINUE
         ELSE
            DO 20 LFIELD = LFLD1,LFLD2
               CALL OBDSKC (IMAGE(LFIELD),  DISKO(LFIELD), CNOO(LFIELD),
     *            IERR)
               CCDISK(LFIELD) = DISKO(LFIELD)
               CCCNO(LFIELD) = CNOO(LFIELD)
               IF (IERR.NE.0) GO TO 990
 20            CONTINUE
            END IF
C                                       Scratch file numbers for work?
C                                       They may not exist.
         MSGSUP = 32000
         IF (WORK1.EQ.' ') WORK1 = 'Work grid 1 for OUVIMG'
         SCRGRD = 0
         CALL IMGET (WORK1, 'SCRCNO', TYPE, DIM, SCRGRD, CDUMMY, IERR)
         MAKGRD = .FALSE.
         IF ((IERR.EQ.1) .OR. (SCRGRD.LE.0)) THEN
            IERR = 0
            SCRGRD = -1
            MAKGRD = .TRUE.
            END IF
         IF (IERR.NE.0) GO TO 995
         IF (WORK2.EQ.' ') WORK2 = 'Work grid 2 for OUVIMG'
         SCRWRK = 0
         CALL IMGET (WORK2, 'SCRCNO', TYPE, DIM, SCRWRK, CDUMMY, IERR)
         MAKWRK = .FALSE.
         IF ((IERR.EQ.1) .OR. (SCRWRK.LE.0)) THEN
            IERR = 0
            SCRWRK = -1
            MAKWRK = .TRUE.
            END IF
         IF (IERR.NE.0) GO TO 995
         MSGSUP = MSGSAV
C                                       uvdata Disk, CNO
         CALL OBDSKC (UVDATA, DISKI, CNOI, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       DFIL.INC scratch file number?
         MSGSUP = 32000
         SCRNO = 0
         CALL OUVGET (UVDATA, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
C                                       Scratch file
         ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
            DISKI = 0
            CNOI = SCRNO
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Control information
C                                       Image size
         MSGSUP = MSGSAV
         CALL IMGET (IMAGE(1), 'IMSIZE', TYPE, DIM, FLDSZ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         NXMAX = 0
         NYMAX = 0
         DO 30 I = 1,NFIELD
            NXMAX = MAX (NXMAX, FLDSZ(1,I))
            NYMAX = MAX (NYMAX, FLDSZ(2,I))
            LNEED = 2*FLDSZ(2,I)*(FLDSZ(1,I)/2+11)
            NEED = MAX (NEED, LNEED)
            TNEED = TNEED + LNEED
 30         CONTINUE
C                                       CELLSIZE
         CALL IMGET (IMAGE(1), 'CELLSIZE', TYPE, DIM, CELLSG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       X shift
         MSGSUP = 32000
         CALL IMGET (IMAGE(1), 'RASHIFT', TYPE, DIM, XSHIFT, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL RFILL (MAXFLD, 0.0, XSHIFT)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y shift
         CALL IMGET (IMAGE(1), 'DECSHIFT', TYPE, DIM, YSHIFT, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL RFILL (MAXFLD, 0.0, YSHIFT)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Center X position
         CALL IMGET (IMAGE(1), 'CENTERX', TYPE, DIM, ICNTRX, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL FILL (MAXFLD, 0, ICNTRX)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Center Y position
         CALL IMGET (IMAGE(1), 'CENTERY', TYPE, DIM, ICNTRY, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL FILL (MAXFLD, 0, ICNTRY)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X convolving function
         CALL IMGET (IMAGE(1), 'CTYPX', TYPE, DIM, CTYPX, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CTYPX = 0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X convolving function parms
         CALL IMGET (IMAGE(1), 'XPARM', TYPE, DIM, XPARM, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL RFILL (10, 0.0, XPARM)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y convolving function
         CALL IMGET (IMAGE(1), 'CTYPY', TYPE, DIM, CTYPY, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CTYPY = 0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y convolving function parms
         CALL IMGET (IMAGE(1), 'YPARM', TYPE, DIM, YPARM, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL RFILL (10, 0.0, YPARM)
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Beam size - no default
         IF (DOBEAM) THEN
            NEED = 0
            TNEED = 0
            DO 40 LFIELD = LFLD1,LFLD2
               BFIELD = MAX (1, LFIELD)
               IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
C                                       field dependent taper: 2 beams
C                                       if taper used here for
C                                       multi-scale beams
               MSGSUP = 32000
               CALL IMGET (IMAGE(BFIELD), 'UVTAPER', TYPE, DIM,
     *            UVTAPR, CDUMMY, IERR)
               MSGSUP = MSGSAV
               NLOOP = 1
               IF ((IERR.EQ.0) .AND. ((UVTAPR(1).GT.0.0) .OR.
     *            (UVTAPR(2).GT.0.0))) NLOOP = 2
               IERR = 0
               CALL IMGET (BEAM(BFIELD), 'IMSIZE', TYPE, DIM, BMSIZ,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
               NXBEM(LFIELD) = BMSIZ(1)
               NYBEM(LFIELD) = BMSIZ(2)
               LNEED = 2*BMSIZ(2)*(BMSIZ(1)/2+11) * NLOOP
               TNEED = TNEED + LNEED
               NEED = MAX (NEED, LNEED)
               NXMAX = MAX (NXMAX, BMSIZ(1))
               NYMAX = MAX (NYMAX, BMSIZ(2))
 40            CONTINUE
            END IF
C                                       Zero spacing flux
         MSGSUP = 32000
         CALL IMGET (IMAGE(1), 'DOZERO', TYPE, DIM, DOZERO, CDUMMY,
     *      IERR)
         ISZERO = .TRUE.
         IF (IERR.EQ.1) THEN
            IERR = 0
            ISZERO = .FALSE.
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Open for buffer, set selection
         CALL IMGET (IMAGE(1), 'ZEROSP', TYPE, DIM, ZEROSP, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            DOZERO = .FALSE.
            CALL RFILL (5, 0.0, ZEROSP)
            END IF
         IF (.NOT.ISZERO) DOZERO = ZEROSP(5).GT.0.0
         IF (IERR.NE.0) GO TO 995
C                                       TFLUXG
         CALL IMGET (IMAGE(1), 'TFLUXG', TYPE, DIM, TFLUXG, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            TFLUXG = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       get freqs
C                                       Open for buffer, update info
         MSGSUP = MSGSAV
         CALL OUVOPN (UVDATA, 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Frequencies
         CALL UVFRQS (UVDATA, FREQUV, FREQG, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Check data type
         CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (UVTYPE(:1).NE.'U') THEN
            MSGTXT = 'OUVIMG (FFT) DOES NOT WORK FOR UV DATA OF TYPE '''
     *         // UVTYPE // ''''
            IERR = 8
            GO TO 995
            END IF
         CALL OUVCLO (UVDATA, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Max value abs(U)
         MSGSUP = 32000
         CALL OUVGET (UVDATA, 'MAXBLINE', TYPE, DIM, MAXUU, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            MAXUU = -1.
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       UV RANGE
         CALL OGET (UVDATA, 'UVRANGE', TYPE, DIM, UVRNG, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            UVRNG(1) = 0.0
            UVRNG(2) = 1.0E10
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Guardband
         CALL OGET (UVDATA, 'GUARDBND', TYPE, DIM, GUARDB, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            GUARDB(1) = 0.0
            GUARDB(2) = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Get number of channels in the
C                                       output image.
         MSGSUP = MSGSAV
         CALL ARDGET (IMAGE(1), 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         NUCHOU = MAX (1, NAXIS(3))
C                                       Normalization factor
         BFIELD = LFLD1
         IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
         MSGSUP = 32000
         CALL IMGET (BEAM(BFIELD), 'SWEIGHTS', TYPE, DIM, SWTS, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            SWTS = FBLANK
            END IF
         IF (IERR.NE.0) GO TO 995
         CALL IMGET (BEAM(BFIELD), 'SUMWTS', TYPE, DIM, SUMWTS, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            CALL RFILL (NUCHOU, SWTS, SUMWTS)
            END IF
         IF (IERR.NE.0) GO TO 995
         MSGSUP = MSGSAV
C                                       LREC
         CALL UVDGET (UVDATA, 'LREC', TYPE, DIM, LLREC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Line or bandwidth synthesis
         NCHAVG = NCHAN
C                                       "grab AP"
         BNEED = MAX (100*LLREC, JBUFSZ/2) + 1900
         I1 = NEED + BNEED
         I1 = I1 / 1024 + 1
         FAST = I1.LE.KAPWRD
C                                       open AP for what we can get
         IF (FAST) THEN
            I1 = (TNEED/1024) + 1
            I1 = I1 + (BNEED/1024) + 1
            ALL = I1.LE.KAPWRD
            CALL QINIT (I1, I2, I3)
            IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               MSGTXT = 'OUVIMG: FAILED TO GET NEEDED MEMORY'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 990
               END IF
            ALL = I1.LE.PSAPNW
         ELSE
            CALL QINIT (I1, I2, I3)
            IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               MSGTXT = 'OUVIMG: FAILED TO GET NEEDED MEMORY'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 990
               END IF
            END IF
         CALL APOBJ ('OPEN', 'OUVIMG', IERR)
         IF (IERR.NE.0) GO TO 990
         APOPEN = .TRUE.
C                                       Decide about sorting
         IF (.NOT.FAST) THEN
            MSGSUP = 32000
            CALL GRDFIT (2, LLREC, FREQG, MAXUU, '    ', NEED, IERR)
            MSGSUP = MSGSAV
            IF ((IERR.LT.0) .OR. (IERR.EQ.1)) THEN
               CALL QRLSE
               I1 = NEED / 1024 + 10
               CALL QINIT (I1, I2, I3)
               IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
                  MSGTXT = 'OUVIMG: FAILED TO GET NEEDED MEMORY'
                  CALL MSGWRT (8)
                  IERR = 8
                  GO TO 990
                  END IF
               CALL GRDFIT (2, LLREC, FREQG, MAXUU, '    ', NEED, IERR)
               END IF
            IF (IERR.GT.1) THEN
               GO TO 990
            ELSE IF (IERR.EQ.1) THEN
               MSGTXT = 'PROBLEM AS POSED WILL NOT FIT EVEN IF DATA ARE'
     *            // ' SORTED'
               GO TO 995
C                                       Do sort
            ELSE IF (IERR.LT.0) THEN
               CALL UVDGET (UVDATA, 'SORTORD', TYPE, DIM, DUMMY, SORD,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               IF (SORD(1:1).NE.'X') THEN
                  MSGTXT = 'OUVIMG: Sorting data to make them fit'
                  CALL MSGWRT (2)
                  SORD = 'XY'
                  CALL OUVPUT (UVDATA, 'SORT', OOACAR, DIM, DUMMY, SORD,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  DIM(1) = 1
                  CALL OUVPUT (UVDATA, 'ROTATE', OOARE, DIM, 0.0,
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL OUVSRT (UVDATA, UVDATA, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               END IF
            END IF
         CALL OBOPEN (UVDATA, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get buffer number
         CALL OBINFO (UVDATA, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Visibility record size.
         CALL UVDGET (UVDATA, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Input file catalog header
         CALL OUVCGT (UVDATA, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL RCOPY (256, CATR, CATUVR)
         IF (DOBEAM) THEN
            DOINIT = .TRUE.
C                                       disk-based imaging
            IF (.NOT.FAST) THEN
               DO 90 LFIELD = LFLD1,LFLD2
                  BFIELD = MAX (1, LFIELD)
                  IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
C                                       skip if onebem
                  IF (BFIELD.LT.LFIELD) THEN
                     BEMMAX(LFIELD) = BEMMAX(BFIELD)
                     GO TO 90
                     END IF
C                                       field dependent taper
                  MSGSUP = 32000
                  CALL IMGET (IMAGE(LFIELD), 'UVTAPER', TYPE, DIM,
     *               UVTAPR, CDUMMY, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.EQ.1) THEN
                     IERR = 0
                     UVTAPR(1) = 0.0
                     UVTAPR(2) = 0.0
                     END IF
                  IF (IERR.NE.0) GO TO 995
C                                       bea, is convolved twice with the
C                                       model size, image but once
C                                       Must get SUMwts for 1 time
                  TAPERU = MAX (0.0, UVTAPR(1))
                  TAPERV = MAX (0.0, UVTAPR(2))
                  DOTAPE = (TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0)
                  NLOOP = 1
                  IF (DOTAPE) NLOOP = 2
                  DO 80 LOOP = 1,NLOOP
C                                       get sum weights
                     MSGSUP = 32000
                     CALL IMGET (BEAM(BFIELD), 'SWEIGHTS', TYPE, DIM,
     *                  SWTS, CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        SWTS = FBLANK
                        END IF
                     IF (IERR.NE.0) GO TO 995
                     MSGSUP = 32000
                     CALL IMGET (BEAM(BFIELD), 'SUMWTS', TYPE, DIM,
     *                  SUMWTS, CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        CALL RFILL (NUCHOU, SWTS, SUMWTS)
                        END IF
                     IF (IERR.NE.0) GO TO 995
C                                       Make beam
                     DOBM = .FALSE.
C                                       Disk-based imaging
                     CALL COPY (256, CATUVR, CATBLK)
                     CALL MAKMAP (-LFIELD, DISKI, CNOI, DISKO(LFIELD),
     *                  CNOO(LFIELD), SCRGRD, SCRWRK, CHAN, IMCHAN,
     *                  DOCREA, DOINIT, DOBM, DOSEL, DOGCOR, CHINC,
     *                  JBUFSZ, OBUFFR(1,BUFNO), IERR)
                     IF (IERR.NE.0) GO TO 990
                     SUMWTS(IMCHAN) = BEMMAX(LFIELD)
                     SWTS = BEMMAX(LFIELD)
C                                       MAKMAP will write CATBLK - save.
                     CALL IMGOPN (BEAM(BFIELD), 'READ', IERR)
                     IF (IERR.NE.0) GO TO 990
                     CALL OBHGET (BEAM(BFIELD), CAT2R, IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Save max, min
                     CAT2R(KRDMN) = CATR(KRDMN)
                     CAT2R(KRDMX) = CATR(KRDMX)
                     CALL OBHPUT (BEAM(BFIELD), CAT2R, IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Close image
                     CALL IMGCLO (BEAM(BFIELD), IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Save normalization
                     IF (LOOP.EQ.1) THEN
                        DIM(1) = NUCHOU
                        CALL IMPUT (BEAM(BFIELD), 'SUMWTS', OOARE, DIM,
     *                     SUMWTS, CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
                        DIM(1) = 1
                        CALL IMPUT (BEAM(BFIELD), 'SWEIGHTS', OOARE,
     *                     DIM, SWTS, CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
C                                       Fit beam on first go round
                        CALL OUBFIT (LFIELD, IMAGE(LFIELD),
     *                     BEAM(BFIELD), IMCHAN, IERR)
C                                       double taper for final beam
C                                       image
                        TAPERU = TAPERU / SQRT (2.0)
                        TAPERV = TAPERV / SQRT (2.0)
                        END IF
 80                  CONTINUE
 90               CONTINUE
C                                       in core imaging
            ELSE
               JFLD2 = LFLD1 - 1
 100           JFLD1 = JFLD2 + 1
               IF (JFLD1.LE.LFLD2) THEN
C                                       how much can we do
                  APSIZE = 1024 * PSAPNW
                  TNEED = BNEED
                  I1 = 0
                  DO 110 LFIELD = JFLD1,LFLD2
                     BFIELD = MAX (1, LFIELD)
                     IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES)*NFPRES + 1
C                                       skip if onebem
                     IF (BFIELD.LT.LFIELD) GO TO 110
C                                       field dependent taper
                     MSGSUP = 32000
                     CALL IMGET (IMAGE(LFIELD), 'UVTAPER', TYPE, DIM,
     *                  UVTAPR, CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        UVTAPR(1) = 0.0
                        UVTAPR(2) = 0.0
                        END IF
                     IF (IERR.NE.0) GO TO 995
C                                       bea, is convolved twice with the
C                                       model size, image but once
C                                       Must get SUMwts for 1 time
                     TAPERU = MAX (0.0, UVTAPR(1))
                     TAPERV = MAX (0.0, UVTAPR(2))
                     DOTAPE = (TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0)
                     NLOOP = 1
                     IF (DOTAPE) NLOOP = 2
                     LNEED = (2 * NYBEM(LFIELD) * (NXBEM(LFIELD)/2+8))
     *                  * NLOOP
                     IF ((LNEED+TNEED.GT.APSIZE) .OR.
     *                  (I1+NLOOP.GT.MXIMAG)) THEN
                        JFLD2 = LFIELD - 1
                        GO TO 120
                     ELSE
                        TNEED = TNEED + LNEED
                        I1 = I1 + 1
                        FIELDS(I1) = -LFIELD
                        TAPERS(1,I1) = TAPERU
                        TAPERS(2,I1) = TAPERV
                        IF (NLOOP.EQ.2) THEN
                           I1 = I1 + 1
                           FIELDS(I1) = -LFIELD - 1000000
                           TAPERS(1,I1) = TAPERU / SQRT (2.0)
                           TAPERS(2,I1) = TAPERV / SQRT (2.0)
                           END IF
                        END IF
 110                 CONTINUE
                  JFLD2 = LFLD2
C                                       Imaging all in memory
 120              WRITE (MSGTXT,1120) JFLD1, JFLD2
                  IF (JFLD2.NE.JFLD1) CALL MSGWRT (2)
                  CALL IMGMEM (I1, FIELDS, TAPERS, DISKI, CNOI, CHAN,
     *               IMCHAN, CATUVR, DOGCOR, 0, CHINC, JBUFSZ,
     *               OBUFFR(1,BUFNO), XTREMA, BEAMS, IERR)
                  IF (IERR.NE.0) GO TO 990
C                                       loop over fields included
                  DO 130 I = 1,I1
                     LFIELD = ABS (FIELDS(I))
                     IF (LFIELD.GT.1000000) LFIELD = LFIELD - 1000000
                     BFIELD = MAX (1, LFIELD)
                     IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES)*NFPRES + 1
C                                       get sum weights
                     MSGSUP = 32000
                     CALL IMGET (BEAM(BFIELD), 'SWEIGHTS', TYPE, DIM,
     *                  SWTS, CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        SWTS = FBLANK
                        END IF
                     IF (IERR.NE.0) GO TO 995
                     MSGSUP = 32000
                     CALL IMGET (BEAM(BFIELD), 'SUMWTS', TYPE, DIM,
     *                  SUMWTS, CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        CALL RFILL (NUCHOU, SWTS, SUMWTS)
                        END IF
                     IF (IERR.NE.0) GO TO 995
                     SUMWTS(IMCHAN) = BEMMAX(LFIELD)
                     SWTS = BEMMAX(LFIELD)
C                                       MAKMAP/IMGMEM will write CATBLK
C                                       - save.
                     CALL IMGOPN (BEAM(BFIELD), 'READ', IERR)
                     IF (IERR.NE.0) GO TO 990
                     CALL OBHGET (BEAM(BFIELD), CAT2R, IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Save max, min
                     CAT2R(KRDMN) = XTREMA(1,I)
                     CAT2R(KRDMX) = XTREMA(2,I)
                     CALL OBHPUT (BEAM(BFIELD), CAT2R, IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Close image
                     CALL IMGCLO (BEAM(BFIELD), IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Save normalization
                     IF (FIELDS(I).GT.-1000000) THEN
                        DIM(1) = NUCHOU
                        CALL IMPUT (BEAM(BFIELD), 'SUMWTS', OOARE, DIM,
     *                     SUMWTS, CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
                        DIM(1) = 1
                        CALL IMPUT (BEAM(BFIELD), 'SWEIGHTS', OOARE,
     *                     DIM, SWTS, CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
C                                       write out beam parameters
                        BEAMS(1,I) = BEAMS(1,I) / 3600.0
                        BEAMS(2,I) = BEAMS(2,I) / 3600.0
                        DIM(1) = 1
                        DIM(2) = 1
                        CALL IMPUT (IMAGE(LFIELD), 'BEAM.BMAJ', OOARE,
     *                     DIM, BEAMS(1,I), CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 995
                        CALL IMPUT (IMAGE(LFIELD), 'BEAM.BMIN', OOARE,
     *                     DIM, BEAMS(2,I), CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 995
                        CALL IMPUT (IMAGE(LFIELD), 'BEAM.BPA', OOARE,
     *                     DIM, BEAMS(3,I), CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 995
                        END IF
 130                 CONTINUE
                  GO TO 100
                  END IF
               END IF
C                                       Save conv fn
            DIM(1) = 1
            DIM(2) = 1
            CALL IMPUT (IMAGE(1), 'CTYPX', OOAINT, DIM, CTYPX, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL IMPUT (IMAGE(1), 'CTYPY', OOAINT, DIM, CTYPY, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            DIM(1) = 10
            CALL IMPUT (IMAGE(1), 'XPARM', OOARE, DIM, XPARM, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL IMPUT (IMAGE(1), 'YPARM', OOARE, DIM, YPARM, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Made scratch files?
            IF ((MAKGRD) .AND. (.NOT.FAST)) THEN
               MAKGRD = .FALSE.
               CALL OBSCNF (WORK1, SCRGRD, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((MAKWRK) .AND. (.NOT.FAST)) THEN
               MAKWRK = .FALSE.
               CALL OBSCNF (WORK2, SCRWRK, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       FFT image
         ELSE
C           DOINIT = .FALSE.
C                                       using disk
            IF (.NOT.FAST) THEN
               DO 190 LFIELD = LFLD1,LFLD2
C                                       field dependent taper
                  MSGSUP = 32000
                  CALL IMGET (IMAGE(LFIELD), 'UVTAPER', TYPE, DIM,
     *               UVTAPR, CDUMMY, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.EQ.1) THEN
                     IERR = 0
                     UVTAPR(1) = 0.0
                     UVTAPR(2) = 0.0
                     END IF
                  IF (IERR.NE.0) GO TO 995
                  TAPERU = MAX (0.0, UVTAPR(1))
                  TAPERV = MAX (0.0, UVTAPR(2))
                  DOTAPE = (TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0)
C                                       Is it small enough for fast
C                                       imaging?
                  NX = FLDSZ(1,LFIELD)
                  NY = FLDSZ(2,LFIELD)
C                                       get sum weights
                  BFIELD = LFIELD
                  IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
                  CALL IMGET (BEAM(BFIELD), 'SWEIGHTS', TYPE, DIM, SWTS,
     *               CDUMMY, IERR)
                  IF (IERR.EQ.1) THEN
                     IERR = 0
                     SWTS = FBLANK
                     END IF
                  IF (IERR.NE.0) GO TO 995
                  IF ((SWTS.NE.FBLANK) .AND. (SWTS.GT.0.0))
     *               MSGSUP = 32000
                  CALL IMGET (BEAM(BFIELD), 'SUMWTS', TYPE, DIM, SUMWTS,
     *               CDUMMY, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.EQ.1) THEN
                     IERR = 0
                     CALL RFILL (NUCHOU, SWTS, SUMWTS)
                     END IF
                  IF (IERR.NE.0) GO TO 995
                  BEMMAX(LFIELD) = SUMWTS(IMCHAN)
                  DOBM = BEMMAX(LFIELD).EQ.FBLANK
                  WDOBM = DOBM
                  CALL COPY (256, CATUVR, CATBLK)
                  CALL MAKMAP (LFIELD, DISKI, CNOI, DISKO(LFIELD),
     *               CNOO(LFIELD), SCRGRD, SCRWRK, CHAN, IMCHAN,
     *               DOCREA, DOINIT, DOBM, DOSEL, DOGCOR, CHINC,
     *               JBUFSZ, OBUFFR(1,BUFNO), IERR)
                  IF (IERR.NE.0) GO TO 990
                  SUMWTS(IMCHAN) = BEMMAX(LFIELD)
C                                       save actual field max min
                  DIM(1) = 1
                  DIM(2) = 1
                  CALL IMPUT (IMAGE(LFIELD), 'FIELDMIN', OOARE, DIM,
     *               FLDMIN(LFIELD),  CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL IMPUT (IMAGE(LFIELD), 'FIELDMAX', OOARE, DIM,
     *               FLDMAX(LFIELD),  CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
C                                       Save normalization
                  IF (WDOBM) THEN
                     DIM(1) = NUCHOU
                     CALL IMPUT (BEAM(BFIELD), 'SUMWTS', OOARE, DIM,
     *                  SUMWTS, CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 990
                     END IF
C                                       MAKMAP/IMGMEM will write CATBLK
C                                       - save.
                  CALL IMGOPN (IMAGE(LFIELD), 'READ', IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL OBHGET (IMAGE(LFIELD), CAT2R, IERR)
                  IF (IERR.NE.0) GO TO 990
C                                       Save max, min
                  CAT2R(KRDMN) = CATR(KRDMN)
                  CAT2R(KRDMX) = CATR(KRDMX)
                  CALL OBHPUT (IMAGE(LFIELD), CAT2R, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL IMGCLO (IMAGE(LFIELD), IERR)
                  IF (IERR.NE.0) GO TO 990
 190              CONTINUE
C                                       do in memory
            ELSE
               JFLD2 = LFLD1 - 1
               I3 = 0
 200           JFLD1 = JFLD2 + 1
               I3 = I3 + 1
               IF (JFLD1.LE.LFLD2) THEN
C                                       how much can we do
                  APSIZE = 1024 * PSAPNW
                  TNEED = BNEED
                  I1 = 0
C                                       consecutive fields
                  IF (NCHOIC.LE.0) THEN
                     DO 210 LFIELD = JFLD1,LFLD2
C                                       field dependent taper
                        MSGSUP = 32000
                        CALL IMGET (IMAGE(LFIELD), 'UVTAPER', TYPE, DIM,
     *                     UVTAPR, CDUMMY, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.EQ.1) THEN
                           IERR = 0
                           UVTAPR(1) = 0.0
                           UVTAPR(2) = 0.0
                           END IF
                        IF (IERR.NE.0) GO TO 995
                        TAPERU = MAX (0.0, UVTAPR(1))
                        TAPERV = MAX (0.0, UVTAPR(2))
                        DOTAPE = (TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0)
                        NX = FLDSZ(1,LFIELD)
                        NY = FLDSZ(2,LFIELD)
                        LNEED = (2 * NY * (NX/2+8))
                        IF ((LNEED+TNEED.GT.APSIZE) .OR.
     *                     (I1+1.GT.MXIMAG))THEN
                           JFLD2 = LFIELD - 1
                           GO TO 220
                        ELSE
                           TNEED = TNEED + LNEED
                           I1 = I1 + 1
                           FIELDS(I1) = LFIELD
                           TAPERS(1,I1) = TAPERU
                           TAPERS(2,I1) = TAPERV
                           END IF
 210                    CONTINUE
C                                       outside list
                  ELSE
                     DO 215 I2 = I3,NCHOIC
                        LFIELD = IFIELD(I2) - 1000000
C                                       field dependent taper
                        MSGSUP = 32000
                        CALL IMGET (IMAGE(LFIELD), 'UVTAPER', TYPE, DIM,
     *                     UVTAPR, CDUMMY, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.EQ.1) THEN
                           IERR = 0
                           UVTAPR(1) = 0.0
                           UVTAPR(2) = 0.0
                           END IF
                        IF (IERR.NE.0) GO TO 995
                        TAPERU = MAX (0.0, UVTAPR(1))
                        TAPERV = MAX (0.0, UVTAPR(2))
                        DOTAPE = (TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0)
                        NX = FLDSZ(1,LFIELD)
                        NY = FLDSZ(2,LFIELD)
                        LNEED = (2 * NY * (NX/2+8))
                        IF ((LNEED+TNEED.GT.APSIZE) .OR.
     *                     (I1+1.GT.MXIMAG))THEN
                           JFLD2 = LFIELD - 1
                           GO TO 220
                        ELSE
                           TNEED = TNEED + LNEED
                           I1 = I1 + 1
                           I3 = I3 + 1
                           FIELDS(I1) = LFIELD
                           TAPERS(1,I1) = TAPERU
                           TAPERS(2,I1) = TAPERV
                           END IF
 215                    CONTINUE
                     END IF
                  JFLD2 = LFLD2
C                                       get sum weights
 220              IF (NCHOIC.GT.0) THEN
                     WRITE (MSGTXT,1220) (FIELDS(I), I = 1,I1)
                  ELSE
                     WRITE (MSGTXT,1221) JFLD1, JFLD2
                     END IF
                  IF (JFLD2.NE.JFLD1) CALL MSGWRT (2)
                  DO 230 I = 1,I1
                     LFIELD = FIELDS(I)
                     BFIELD = MAX (1, LFIELD)
                     IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES)*NFPRES + 1
                     CALL IMGET (BEAM(BFIELD), 'SWEIGHTS', TYPE, DIM,
     *                  SWTS, CDUMMY, IERR)
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        SWTS = FBLANK
                        END IF
                     IF (IERR.NE.0) GO TO 995
                     IF ((SWTS.NE.FBLANK) .AND. (SWTS.GT.0.0))
     *                  MSGSUP = 32000
                     CALL IMGET (BEAM(BFIELD), 'SUMWTS', TYPE, DIM,
     *                  SUMWTS, CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.EQ.1) THEN
                        IERR = 0
                        CALL RFILL (NUCHOU, SWTS, SUMWTS)
                        END IF
                     IF (IERR.NE.0) GO TO 995
                     BEMMAX(LFIELD) = SUMWTS(IMCHAN)
                     IF (BEMMAX(LFIELD).EQ.FBLANK) THEN
                        WRITE (MSGTXT,1210) LFIELD
                        CALL MSGWRT (8)
                        IERR = 8
                        GO TO 990
                        END IF
 230                 CONTINUE
                  CALL IMGMEM (I1, FIELDS, TAPERS, DISKI, CNOI, CHAN,
     *               IMCHAN, CATUVR, DOGCOR, 0,CHINC, JBUFSZ,
     *               OBUFFR(1,BUFNO), XTREMA, BEAMS, IERR)
                  IF (IERR.NE.0) GO TO 990
                  DO 240 I = 1,I1
                     LFIELD = FIELDS(I)
C                                       save actual field max min
                     DIM(1) = 1
                     DIM(2) = 1
                     CALL IMPUT (IMAGE(LFIELD), 'FIELDMIN', OOARE, DIM,
     *                  FLDMIN(LFIELD),  CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 990
                     CALL IMPUT (IMAGE(LFIELD), 'FIELDMAX', OOARE, DIM,
     *                  FLDMAX(LFIELD),  CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       MAKMAP/IMGMEM will write CATBLK
C                                       - save.
                     CALL IMGOPN (IMAGE(LFIELD), 'READ', IERR)
                     IF (IERR.NE.0) GO TO 990
                     CALL OBHGET (IMAGE(LFIELD), CAT2R, IERR)
                     IF (IERR.NE.0) GO TO 990
C                                       Save max, min
                     CAT2R(KRDMN) = XTREMA(1,1)
                     CAT2R(KRDMX) = XTREMA(2,1)
                     CALL OBHPUT (IMAGE(LFIELD), CAT2R, IERR)
                     IF (IERR.NE.0) GO TO 990
                     CALL IMGCLO (IMAGE(LFIELD), IERR)
                     IF (IERR.NE.0) GO TO 990
 240                 CONTINUE
                  GO TO 200
                  END IF
               END IF
            END IF
C                                       Close uvdata object
         CALL APOBJ ('FREE', 'OUVIMG', LERR)
         APOPEN = .FALSE.
         CALL OBCLOS (UVDATA, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       DFT imaging
      ELSE IF (FTTYPE.EQ.'DFT ') THEN
         CALL ODFT (UVDATA, CHAN, NCHAN, IMCHAN, IMAGE(1), BEAM(1),
     *      DOBEAM, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Unknown imaging type
      ELSE
         MSGTXT = 'OUVIMG: UNKNOWN IMAGING TYPE: ' // FTTYPE
         IERR = 2
         GO TO 995
         END IF
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Reveal suppressed message
 995  MSGSUP = MSGSAV
      CALL MSGWRT (7)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUVIMG', LERR)
         END IF
      IF (DOBEAM) THEN
         MSGTXT = 'OUVIMG: MAKING BEAM ' // BEAM(1)
      ELSE
         MSGTXT = 'OUVIMG: MAKING IMAGE ' // IMAGE(1)
         END IF
      CALL MSGWRT (7)
      MSGTXT = 'OUVIMG: FROM ' // UVDATA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('Making beams for fields',I5,' through',I5)
 1210 FORMAT ('OUVIMG: FIELD',I5,' LACKS A REQUIRED BEAM SCALING')
 1220 FORMAT ('Imaging fields',10I5)
 1221 FORMAT ('Making images for fields',I5,' through',I5)
      END
      SUBROUTINE OUVSRT (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Sorts uv data with optional rotation of (u,v) coordinates.
C   Copies all descriptive (AN, FQ, SU) tables.
C      Uses the "AP" to presort blocks of data and uses a disk based
C   merge sort if necessary.
C   Inputs:
C      UVIN    C*?  Name of input uvdata.
C      UVOUT   C*?  Name of output uv data. WIll be created if
C                   necessary.
C   Inputs from UVIN: (defaults where possible)
C      SORT    C*2  desired sort order keys: (default 'TB')
C                   T=time, B=baseline, X=abs (U), Y=abs (V)
C      ROTATE  R    U, V rotation in deg. (default 0.0)
C   Output:
C      IERR    I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   NWAY
C                                       NWAY = number of streams in
C                                       merge sort (min=2)
      PARAMETER (NWAY = 2)
      INTEGER   DISKI, CNOI, DISKO, CNOO, BUFNO1, BUFNO2, MSGSAV, SCRNO,
     *   TYPE, DIM(7), JBUFSZ, I, COUNT, BIF, EIF, BCHAN, ECHAN, NBUF,
     *   LUNST(NWAY,2), DUMMY, LERR, CATIN(256)
      LOGICAL   NEWOUT, INOUT, APOPEN, ISICMP, ISOCMP
      CHARACTER TEMPUV*32, STOKES*4, SORT*2, CDUMMY*1
      REAL      ROTATE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      APOPEN = .FALSE.
C                                       Is input = output?
      INOUT = UVIN.EQ.UVOUT
      MSGSAV = MSGSUP
C                                       Disk, CNO
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save and reset selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output may not exist
      MSGSUP = 32000
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DISKO = 0
         CNOO = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       DFIL.INC scratch file number.
      SCRNO = 0
      CALL OUVGET (UVIN, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
C                                       Scratch file
         DISKI = 0
         CNOI = SCRNO
C                                       Clear any flags
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (UVIN, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
         IERR = 0
         CALL OUCCLR (UVIN, IERR)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Output
      SCRNO = 0
      MSGSUP = 32000
      CALL OUVGET (UVOUT, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
         DISKO = 0
         CNOO = SCRNO
      END IF
      IF (IERR.NE.0) GO TO 995
C                                       Does output exist
      NEWOUT = (DISKO.LE.0) .AND. (CNOO.LE.0)
C                                       Make temporary objects for
C                                       buffer
      MSGSUP = MSGSAV
      IF (INOUT) THEN
         TEMPUV = 'Temporary uv data for OUVSRT'
         CALL OBCREA (TEMPUV, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open for buffers, update info
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVIN, 'ISCOMP', TYPE, DIM, ISICMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      ISOCMP = ISICMP
      IF (INOUT) THEN
         CALL OBOPEN (TEMPUV, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Save vis count.
         CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       This funny business to fool the
C                                       I/O which thinks no data will be
C                                       written.
         CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVOUT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVDGET (UVOUT, 'ISCOMP', TYPE, DIM, ISOCMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get buffer numbers
      CALL OBINFO (UVIN, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBINFO (TEMPUV, BUFNO2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OBINFO (UVOUT, BUFNO2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Control info
      MSGSUP = 32000
C                                       Sort order
      CALL OUVGET (UVIN, 'SORT', TYPE, DIM, DUMMY, SORT, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SORT = 'TB'
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Rotation
      CALL OUVGET (UVIN, 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         ROTATE = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSUP = MSGSAV
C                                       Check compression state
      IF (ISICMP.NEQV.ISOCMP) THEN
         MSGTXT = 'OUVSRT: INPUT AND OUTPUT COMPRESSION STATES DIFFER'
         CALL MSGWRT (8)
         IERR = 10
         GO TO 995
         END IF
C                                       Copy descriptive tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OUVCGT (UVIN, CATIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close objects
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INOUT) THEN
         CALL OBCLOS (TEMPUV, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Assign LUNs
      NBUF = NWAY
      DO 110 I = 1,NBUF
         CALL OBLUN (LUNST(I,1), IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OBLUN (LUNST(I,2), IERR)
         IF (IERR.NE.0) GO TO 990
 110     CONTINUE
C                                       Buffer size in "AIPS bytes"
      JBUFSZ = BUFSIZ * 2
C                                       sort
      CALL APOBJ ('OPEN', 'OUVSRT', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL UVSORT (DISKI, CNOI, DISKO, CNOO, SORT, ROTATE, CATIN, NBUF,
     *   LUNST, JBUFSZ, OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('FREE', 'OUVSRT', LERR)
      APOPEN = .FALSE.
C                                       Free LUNs
      DO 120 I = 1,NBUF
         CALL OBLUFR (LUNST(I,1))
         CALL OBLUFR (LUNST(I,2))
 120     CONTINUE
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset VISOFF
      COUNT = 0
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save new sort order
      DIM(1) = LEN (SORT)
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary objects
      IF (INOUT) THEN
         CALL OBFREE (TEMPUV, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Reset UVIN selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       If created output save info
      IF (NEWOUT) CALL OBSCNF (UVOUT, CCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OUVSRT', LERR)
         END IF
      MSGTXT = 'OUVSRT: ERROR SORTING ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUSETF (TFLUX, NFIELD, FLUX)
C-----------------------------------------------------------------------
C   Sets values of total flux densities in DGDS.INC commons
C   Inputs:
C      TFLUX    R    Total flux density (Jy)
C      NFIELD   I    Number of fields
C      FLUX     R(*) Flux density per field
C-----------------------------------------------------------------------
      INTEGER   NFIELD
      REAL      TFLUX, FLUX(NFIELD)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   N
C-----------------------------------------------------------------------
      TFLUXG = TFLUX
      N = MIN (MAXFLD, NFIELD)
      CALL RCOPY (N, FLUX, FLUXG)
C
 999  RETURN
      END
      SUBROUTINE OSDIMG (UVDATA, IMAGE, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Makes a multi-channel image of the single-dish data contained in
C   UVDATA.
C   Inputs:
C      UVDATA   C*(*)    Name of UV data input object
C      IMAGE    C*(*)    Name of image output object
C   Outputs:
C      IERR     I        Error code: 0 okay, checked on input
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), IMAGE*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), DUMMY, MSGSAV, DISKI, CNOI, SCRNO, LUNL,
     *   LERR, DISKO, CNOO, IWT, IMSI(2), BUFNO1, BUFNO2, BUFNO3,
     *   BUFNO4, LBUFSZ
      REAL      RWT, CELLSZ(2), CATMAR(256), MAXCWT, XNLIM, AVGTIM(2),
     *   PANGL, ZANGL, BEMSZ(2)
      CHARACTER TEMPUV*32, TEMPIM*32, UVTYPE*2, CDUMMY*1, CHTYPE*4,
     *   CNAME*8, KEYW*8
      DOUBLE PRECISION RASAVE, DESAVE, CATMAD(128)
      LOGICAL   APOPEN
      INCLUDE 'INCS:DSDG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      EQUIVALENCE (CATMAP, CATMAR, CATMAD)
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      MSGSAV = MSGSUP
      APOPEN = .FALSE.
      LBUFSZ = 2 * BUFSIZ
C                                       STEP 1: prepare uv data commons
C                                       Open for buffer, update info
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((UVTYPE.NE.'SD') .AND. (UVTYPE.NE.'SB')) THEN
         MSGTXT = 'OSDIMG DOES NOT WORK FOR UV DATA OF TYPE '''
     *      // UVTYPE // ''''
         IERR = 8
         GO TO 995
         END IF
      CALL OBHGET (UVDATA, CATMAP, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
      BEMSZ(1) = CATMAR(KRBMJ) * 3600.
      BEMSZ(2) = CATMAR(KRBMN) * 3600.
C                                       Disk, CNO
C                                       DFIL.INC scratch file number.
      MSGSUP = 32000
      SCRNO = 0
      CALL OUVGET (UVDATA, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Cataloged file
      IF ((IERR.EQ.1) .OR. (SCRNO.LE.0)) THEN
         CALL OBDSKC (UVDATA, DISKI, CNOI, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Scratch file
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
         DISKI = 0
         CNOI = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       STEP 2: create the image
      CHTYPE = 'LINE'
      DIM(1) = LEN (CHTYPE)
      DIM(2) = 1
      CALL OPUT (IMAGE, 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       build header
      CALL U2IDES (UVDATA, IMAGE, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Create
      CALL OOPEN (IMAGE, 'DEST', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       temps for more buffers
      TEMPUV = 'Temporary uv buffer for OSDIMG'
      CALL OBCREA (TEMPUV, 'UVDATA  ', IERR)
      IF (IERR.NE.0) GO TO 995
      TEMPIM = 'Temporary img buffer for OSDIMG'
      CALL OBCREA (TEMPIM, 'IMAGE   ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open for buffers
      CALL OBOPEN (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (TEMPIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get buffer numbers
      CALL OBINFO (IMAGE, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (UVDATA, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TEMPUV, BUFNO3, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TEMPIM, BUFNO4, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Disk, CNO
C                                       DFIL.INC scratch file number.
      MSGSUP = 32000
      SCRNO = 0
      CALL OUVGET (IMAGE, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Cataloged file
      IF ((IERR.EQ.1) .OR. (SCRNO.LE.0)) THEN
         CALL OBDSKC (IMAGE, DISKO, CNOO, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Scratch file
      ELSE IF ((IERR.EQ.0) .AND. (SCRNO.GT.0)) THEN
         DISKO = 0
         CNOO = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       STEP 3: init the common
C                                        SD type, cutoff
      CALL OGET (IMAGE, 'SDTYPE', TYPE, DIM, IWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'SDCUTOFF', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NX = IMSI(1)
      NY = IMSI(2)
      CALL OGET (IMAGE, 'BCHAN', TYPE, DIM, SDBCHN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'ECHAN', TYPE, DIM, SDECHN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'GCOUNT', TYPE, DIM, VISNUM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'CELLSIZE', TYPE, DIM, CELLSZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'CTYPX', TYPE, DIM, CXTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'CTYPY', TYPE, DIM, CYTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'XPARM', TYPE, DIM, XPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'YPARM', TYPE, DIM, YPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       apply defaults and store back
      CALL GRDFIX (CXTYPE, CYTYPE, XPARM, YPARM, CELLSZ, BEMSZ)
      DIM(1) = 1
      CALL OPUT (IMAGE, 'CTYPX', OOAINT, DIM, CXTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (IMAGE, 'CTYPY', OOAINT, DIM, CYTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 10
      CALL OPUT (IMAGE, 'XPARM', OOARE, DIM, XPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (IMAGE, 'YPARM', OOARE, DIM, YPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Catalog header
      CALL OBHGET (IMAGE, CATMAP, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       STEP 4: do it
      CALL APOBJ ('OPEN', 'OSDIMG', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL OBLUN (LUNL, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL SDGRID (LUNL, DISKI, CNOI, DISKO, CNOO, .TRUE., IWT, RWT,
     *   LBUFSZ, OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), LBUFSZ,
     *   OBUFFR(1,BUFNO3), LBUFSZ, OBUFFR(1,BUFNO4), MAXCWT, XNLIM,
     *   IERR)
      CALL OBLUFR (LUNL)
      CALL APOBJ ('FREE', 'OSDIMG', LERR)
      APOPEN = .FALSE.
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (IMAGE, 'MAXCWT', OOARE, DIM, MAXCWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (IMAGE, 'XNLIM', OOARE, DIM, XNLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARSPUT (IMAGE, 'DATAMAX', OOARE, DIM, CATMAR(KRDMX), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARSPUT (IMAGE, 'DATAMIN', OOARE, DIM, CATMAR(KRDMN), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (IMAGE, 'BLANK', OOARE, DIM, CATMAR(KRBLK), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'BMAJ', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (IMAGE, 'BEAM.BMAJ', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'BMIN', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (IMAGE, 'BEAM.BMIN', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'BPA', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (IMAGE, 'BEAM.BPA', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Declare 'BSTHROW' a header
C                                       keyword for the uvdata class
      CNAME = 'UVDATA'
      KEYW = 'BSTHROW'
      CALL OBVHKW (CNAME, KEYW, OOARE, LERR)
      IF (LERR.EQ.0) THEN
         MSGSUP = 32000
         CALL OGET (UVDATA, KEYW, TYPE, DIM, RWT, CDUMMY, LERR)
         MSGSUP = MSGSAV
         END IF
      IF (LERR.EQ.0) THEN
         CNAME = 'IMAGE'
         CALL OBVHKW (CNAME, KEYW, OOARE, LERR)
         END IF
      IF (LERR.EQ.0) THEN
         CALL OPUT (IMAGE, KEYW, TYPE, DIM, RWT, CDUMMY, LERR)
         END IF
C                                       Declare 'PARANGLE' a header
C                                       keyword for the image class.
      RASAVE = RA
      DESAVE = DEC
      IF (LERR.EQ.0) THEN
         CNAME = 'IMAGE'
         KEYW = 'PARANGLE'
         CALL OBVHKW (CNAME, KEYW, OOARE, LERR)
         END IF
C                                       Declare 'ZENANGLE' a header
C                                       keyword for the image class.
      IF (LERR.EQ.0) THEN
         CNAME = 'IMAGE'
         KEYW = 'ZENANGLE'
         CALL OBVHKW (CNAME, KEYW, OOARE, LERR)
         END IF
C
      IF (LERR.EQ.0) CALL IMTAV (UVDATA, AVGTIM, LERR)
C                                       Get parallactic, zenith angles
      RA = CATMAD(KDORA)
      DEC = CATMAD(KDODE)
      IF (LERR.EQ.0) CALL IMPARG (UVDATA, AVGTIM(1), PANGL, ZANGL, IERR)
C                                       Save observing geometry
      IF (LERR.EQ.0) THEN
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (IMAGE, 'PARANGLE', OOARE, DIM, PANGL, CDUMMY, LERR)
         END IF
      IF (LERR.EQ.0) CALL OPUT (IMAGE, 'ZENANGLE', OOARE, DIM, ZANGL,
     *   CDUMMY, LERR)
C                                       Close and write to disk
      RA = RASAVE
      DEC = DESAVE
      CALL OCLOSE (IMAGE, LERR)
      CALL IMCDES (IMAGE, '    ', LERR)
C                                       close other buffers
      CALL OBCLOS (UVDATA, LERR)
      CALL OBCLOS (TEMPUV, LERR)
      CALL OBCLOS (TEMPIM, LERR)
C                                       destroy temporary object
      CALL OBFREE (TEMPUV, LERR)
      CALL OBFREE (TEMPIM, LERR)
      GO TO 999
C                                       Reveal suppressed message
 995  MSGSUP = MSGSAV
      CALL MSGWRT (7)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'OSDIMG', LERR)
         END IF
      MSGTXT = 'OSDIMG: MAKING IMAGE ' // IMAGE
      CALL MSGWRT (7)
      MSGTXT = 'OSDIMG: FROM ' // UVDATA
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMPARG (UVDATA, AVGTIM, PANGL, ZANGL, IERR)
C-----------------------------------------------------------------------
C   Determines the observing geometry from the average time.
C   Inputs:
C      UVDATA C*?  UV residual file from previous CLEAN
C      AVGTIM R Average time in days.
C   Output:
C      PANGL  R Parallactic angle in degrees.
C      ZANGL  R Zenith angle in degrees.
C      IERR   I Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      REAL      AVGTIM, PANGL, ZANGL
      INTEGER   IERR
C
      INTEGER   ANT, SUBBRR, SOURID
      CHARACTER TABLE*32
      REAL      PANG, EL, AZ, HA
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      TABLE = 'Temporary table for IMPARG'
      CALL UV2TAB (UVDATA, TABLE, 'AN', 1, IERR)
C                                       Get parallactic angle
      ANT = 1
      SUBBRR = 1
      SOURID = SOUWAN(1)
      IF (SOURID.LE.0) SOURID = 1
      CALL OSUPAN (TABLE, ANT, SUBBRR, SOURID, AVGTIM, PANG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get elevation
      TIME = AVGTIM
      CALL OSUELV (TABLE, ANT, SUBBRR, SOURID, TIME, HA, EL, AZ, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete object
      CALL TABDES (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert to degrees.
      PANGL = PANG * RAD2DG
C                                       Get zenith angle
      ZANGL = 90.0 - EL * RAD2DG
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMPARG: ERROR DETERMINING OBSERVING GEOMETRY'
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMTAV (UVDATA, AVGTIM, IERR)
C-----------------------------------------------------------------------
C   Determines the average time of a data set. Averages time in GST and
C   then converts back to Solar time on day 0.  This avoids the problems
C   associated with observations spanning several days.
C      NOTE: this routine uses "AP" memory for scratch memory
C   Inputs:
C      UVDATA C*?  UV residual file from previous CLEAN
C   Output:
C      AVGTIM R(2) Average time in days, rms
C      IERR   I    Error code: 0 => ok, checked on input
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      REAL      AVGTIM(2)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   COUNT, JLOCT, TYPE, DIM(3), LERR
      REAL      RP(50), VIS(3,MAXCIF)
      DOUBLE PRECISION GSTIA0, DEGPDY, SUMR, SUMI, TIME, GST0, ROTRAT,
     *   SUMRR, SUMII
      CHARACTER TABLE*32, CDUM*1
      SAVE VIS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IERR.GT.0) GO TO 999
      CALL APOBJ ('OPEN', 'IMTAV', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get time information from AN
C                                       table.
      TABLE = 'Temporary AN table for IMTAV'
      CALL UV2TAB (UVDATA, TABLE, 'AN', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABOPN (TABLE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABGET (TABLE, 'KEY.GSTIA0', TYPE, DIM, GSTIA0, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABGET (TABLE, 'KEY.DEGPDY', TYPE, DIM, DEGPDY, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABCLO (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete object
      CALL TABDES (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert units to radians
      GST0 = GSTIA0 * DG2RAD
      ROTRAT = DEGPDY * DG2RAD
C                                       Open uv data
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Which random parameter
      CALL UVDFND (UVDATA, 1, 'TIME', JLOCT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'IMTAV:TROUBLE FINDING RANDOM PARAMETER TIME'
         CALL MSGWRT (7)
         GO TO 990
         END IF
      SUMR = 0.0D0
      SUMI = 0.0D0
      SUMRR = 0.0D0
      SUMII = 0.0D0
      COUNT = 0
C                                       Loop thru data
 100     CALL UVREAD (UVDATA, RP, VIS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Convert time to GST(radians)
         TIME = RP(JLOCT) * ROTRAT + GST0
C                                       Sum as sine and cosine
         COUNT = COUNT + 1
         SUMR = SUMR + COS (TIME)
         SUMI = SUMI + SIN (TIME)
         SUMRR = SUMRR + (COS (TIME)) ** 2
         SUMII = SUMII + (SIN (TIME)) ** 2
         GO TO 100
 200     IERR = 0
C                                       Close
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Average as cosine and sine to
C                                       deal with periodic nature of GST
      IF (COUNT.GT.0) THEN
         AVGTIM(1) = ATAN2 (SUMI, SUMR+1.0D-20)
         SUMR = SUMR / COUNT
         SUMI = SUMI / COUNT
         SUMR = SUMR * SUMR
         SUMI = SUMI * SUMI
         SUMRR = SUMRR / COUNT - SUMR
         SUMII = SUMII / COUNT - SUMI
         AVGTIM(2) = (SUMI * SUMRR + SUMR * SUMII) / (SUMR + SUMI)
      ELSE
         MSGTXT = 'IMTAV: NO VISIBILITIES'
         CALL MSGWRT (7)
         IERR = 5
         GO TO 990
         END IF
C                                       Convert time back to solar
      AVGTIM(1) = (AVGTIM(1) - GST0)
      IF (AVGTIM(1).LE.0.0) AVGTIM(1) = AVGTIM(1) + TWOPI
      IF (AVGTIM(1).LE.0.0) AVGTIM(1) = AVGTIM(1) + TWOPI
      AVGTIM(1) = AVGTIM(1) / ROTRAT
      AVGTIM(2) = SQRT (AVGTIM(2)) / ROTRAT
      CALL APOBJ ('CLOSE', 'IMTAV', LERR)
      GO TO 999
C                                       Error
 990  CALL APOBJ ('CLOSE', 'IMTAV', LERR)
 995  MSGTXT = 'IMTAV: ERROR AVERAGING TIME FOR ' // UVDATA
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OUBFIT (IFIELD, CNAME, DBNAME, CHANN, IRET)
C-----------------------------------------------------------------------
C   OUBFIT fits an eliptical Gaussian to the dirty beam.
C   If peak of beam is too narrow to fit a default circular Gaussian is
C   used. A grid of up to 5 X 11 points is used for the fit; only points
C   within the half power points are used.  To avoid degenerate
C   cases some of the allowed points are ignored.
C   Solution is by least squares to a linearized gaussian.
C   Inputs:
C      IFIELD  I      Field number
C      CNAME   C*32   Clean image name (1 field)
C      DBNAME  C*32   Dirty beam image name (1 field)
C      CHANN   I      Channel number in output beam
C   Outputs:
C      IRET    I      Return error code 0=>OK, checked on input
C-----------------------------------------------------------------------
      CHARACTER CNAME*(*), DBNAME*(*)
      INTEGER   IFIELD, CHANN, IRET
C
      INTEGER   I, IFLIP, IJK, ILAST, IROW, J, K, L, BLC(7), TRC(7),
     *   DIM(7), ICENX, ICENY, TYPE
      REAL      X(3,3), Y(3), P(3), DX, DY, XFACT, BMJLOC, BMNLOC,
     *   BPALOC, CROTA(7), MROTAT, ROW1(11), CELLS(2)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Existing error?
      IF (IRET.GT.0) GO TO 999
      CELLS(1) = - ABS(CELLSG(1))
      CELLS(2) = CELLSG(2)
      XFACT = ABS (CELLS(1))
      IFLIP = 1
      ICENX = NXBEM(IFIELD) / 2
      ICENY = NYBEM(IFIELD) / 2 + 1
      BLC(1) = ICENX - 5
      BLC(2) = ICENY
      BLC(3) = CHANN
      TRC(1) = ICENX + 5
      TRC(2) = ICENY + 5
      TRC(3) = CHANN
C                                       Get rotation
      CALL IMDGET (CNAME, 'CROTA', TYPE, DIM, CROTA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      MROTAT = CROTA(2)
C                                       Set window
      DIM(1) = 7
      DIM(2) = 1
      DIM(3) = 0
      CALL ARDPUT (DBNAME, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARDPUT (DBNAME, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
      CALL ARROPN (DBNAME, 'READ', IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Zero work arrays.
      DO 20 I = 1,3
         Y(I) = 0.0
         DO 10 J = 1,3
            X(I,J) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Loop through rows.
      DO 70 I = 1,6
C                                       Read row.
         CALL ARREAD (DBNAME, DIM, ROW1, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Loop down row doing alternate
C                                       halves. Go only to first
C                                       decending 0.35 from center.
         DO 65 IJK = 1,2
            IFLIP = - IFLIP
            ILAST = 6 - IFLIP
            DO 60 J = IJK,6
               IROW = 6 + (J-1) * IFLIP
               IF ((ROW1(IROW).LT.0.35) .AND.
     *            (ROW1(IROW).LT.ROW1(ILAST))) GO TO 65
               IF (ROW1(IROW).GE.0.35) THEN
                  ILAST = IROW
C                                       Compute displacements from
C                                       center.
                  DX = IFLIP * (J-1.0) * CELLS(1) / XFACT
                  DY = (1.0-I) * CELLS(2) / XFACT
C                                       Compute partials WRT C1,C2,C3
                  P(1) = DX * DX
                  P(2) = DY * DY
                  P(3) = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                  DO 50 K = 1,3
                     Y(K) = Y(K) - LOG (ROW1(IROW)) * P(K)
                     DO 40 L = 1,3
                        X(K,L) = X(K,L) + P(K) * P(L)
 40                     CONTINUE
 50                  CONTINUE
                  END IF
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
C                                       Close beam
      CALL ARRCLO (DBNAME, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Fit beam
      CALL FITBM (IFIELD, X, Y, CELLS, MROTAT, BMJLOC, BMNLOC, BPALOC)
      BMJLOC = BMJLOC / 3600.0
      BMNLOC = BMNLOC / 3600.0
      DIM(1) = 1
      DIM(2) = 1
      CALL IMPUT (CNAME, 'BEAM.BMAJ', OOARE, DIM, BMJLOC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMPUT (CNAME, 'BEAM.BMIN', OOARE, DIM, BMNLOC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMPUT (CNAME, 'BEAM.BPA', OOARE, DIM, BPALOC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      GO TO 999
C
 995  MSGTXT = 'OUBFIT: ERROR FITTING BEAM ' // DBNAME
      CALL MSGWRT (8)
C
 999  RETURN
      END

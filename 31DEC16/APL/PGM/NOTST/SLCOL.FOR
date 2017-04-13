       PROGRAM SLCOL
C-----------------------------------------------------------------------
C! Collates slice data and models
C# EXT-appl Text Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2001, 2009
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
C   Task to collate slice information.
C   Note: I believe that SLICEs are written to disk as floating point
C   numbers with the inverse of the image scale and offset factors
C   applied.  Thus, this program applies the image scale and offset
C   factors when reading in slice and model data.   If you rescale an
C   image, the new scale factors will therefore be applied to the slice.
C   Inputs:
C 1      USERID (1)      user no.
C 2-4    INNAME (3)      image name
C 5-6    INCLASS (2)     image class
C 7      INSEQ (1)       image sequence number
C 8      INDISK (1)      image disk number
C 9-20   OUTFILE (12)    output file name
C 21     INVERS (1)      start slice number
C 22     NFILES (1)      number of slices
C 23     ZINC (1)        save a slice point every ZINC asecs ('SLVL')
C 24-30  PIXXY (7)       x-y coordinate of reference pixel
C 31     OPCODE (1)      'MODL', AND 'SLVL'
C 32     APARM(1)        <=0 => don't output blank slice points
C                        ('SLVL')
C 33     APARM(2)        <=0 => Overwrite any existing text file
C                        >0  => Append to any existing text file
C
C   Neil Killeen, NRAO, June  1987
C                 UofIll, April 1988
C-----------------------------------------------------------------------
      REAL      RPARM(33)
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IER, IERR, IMSLOT, IMVOL
      INCLUDE 'INCS:DMSG.INC'
C                                       Fixed PPM 1996.09.30: was 33
      DATA NPARMS, PRGNAM /40, 'SLCOL '/
C-----------------------------------------------------------------------
C                                          start up task, get parameters
      CALL TSKBEG (PRGNAM, NPARMS, RPARM(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
      RPARM(1) = NLUSER
C                                          get header from image
      CALL IMHEAD (RPARM, IMSLOT, IMVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                          collate slices
      CALL COLATE (RPARM, IMSLOT, IMVOL, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,3000) IERR
C-----------------------------------------------------------------------
 900  IF (IERR.NE.0) THEN
         IER = 1
         CALL MSGWRT (8)
         CALL CLENUP
      ELSE
         IER = 0
         END IF
      CALL TSKEND (IER)
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING INPUTS,  IERR=', I4)
 2000 FORMAT ('ERROR GETTING IMAGE HEADER, IERR=', I4)
 3000 FORMAT ('ERROR COLLATING SLICES, IERR=', I4)
      END
      SUBROUTINE IMHEAD (RPARM, SLOT, VOL, IERR)
C-----------------------------------------------------------------------
C   Get the header and catalog location of the associated image
C   Input:
C      RPARM     R       array of AIPS input parameters
C   Output:
C      SLOT      I       catalog slot of image
C      VOL       I       volume number (disk) of image
C      IERR      I       error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER IMNAM*36
      HOLLERITH MAP
      REAL   RPARM(*)
      INTEGER   LUN, IERR, CATBLK(256), SLOT, VOL, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      COMMON /HEADS/ CATBLK
      DATA LUN /16/
C-----------------------------------------------------------------------
C                                           set up namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), IMNAM)
C                                           open file
      CALL OPENCF (LUN, IMNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                           get header
      CALL GETHDR (LUN, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                           work out disk and cat slot
      DO 100 I = 1, EFIL
         IF (FILTAB(POLUN, I).EQ.LUN) THEN
            SLOT = FILTAB(POCAT,I)
            VOL  = FILTAB(POVOL,I)
            GO TO 900
         ELSE IF (I.EQ.EFIL) THEN
            WRITE (MSGTXT,3000)
            IERR = 1
            GO TO 900
            END IF
 100     CONTINUE
C                                           report error or
C                                           close image file
 900  IF (IERR.NE.0) THEN
         CALL MSGWRT (8)
      ELSE
         CALL FILCLS (LUN)
         END IF
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMHEAD: ERROR OPENING ASSOCIATED IMAGE, IERR=', I4)
 2000 FORMAT ('IMHEAD: ERROR GETTING FIRST GRADIENT IMAGE HEADER,',
     *        ' IERR=', I4)
 3000 FORMAT ('IMHEAD: ERROR FINDING IMAGE SLOT AND VOLUME')
      END
      SUBROUTINE COLATE (RPARM, SLOT, VOL, IERR)
C-----------------------------------------------------------------------
C     Collate the slice file information and write disk files if
C     requested
C     Input:
C       RPARM     R      AIPS input parameters
C       SLOT      I      cat slot of associated image
C       VOL       I      volume no. of associated image
C     Output:
C       IERR      I      0 => OK
C-----------------------------------------------------------------------
      CHARACTER MODL*4, OPCODE*4, FILNAM*48
      REAL      RPARM(*)
      REAL      ORIGIN(4), XINC, YINC, SINC
      INTEGER   IERR, ISTART, IEND, SLOT, VOL, LUN, IFIND, MSGSAV
      LOGICAL   IGBLA, TERM, APPEND
      INCLUDE 'INCS:DMSG.INC'
C                                           borrow LUN 10 for text file
      DATA LUN, MODL /10, 'MODL'/
C-----------------------------------------------------------------------
C                                           crunch inputs and header
      CALL EXTPAR (RPARM, XINC, YINC, ISTART, IEND, SINC, ORIGIN,
     *   OPCODE, TERM, IGBLA, APPEND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                           open output text file ?
      IF (.NOT.TERM) THEN
         CALL H2CHR (48, 1, RPARM(9), FILNAM)
         IF (.NOT.APPEND) THEN
            MSGSAV = MSGSUP
            MSGSUP = 32000
            CALL ZTXZAP (LUN, FILNAM, IERR)
            MSGSUP = MSGSAV
            END IF
         CALL ZTXOPN ('WRIT', LUN, IFIND, FILNAM, APPEND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
C                                           now branch for OPCODE
      IF (OPCODE.EQ.MODL) THEN
         CALL DOMODL (SLOT, VOL, XINC, YINC, ISTART, IEND, ORIGIN, TERM,
     *      LUN, IFIND, IERR)
      ELSE
         CALL DOSLVL (SLOT, VOL, XINC, YINC, ISTART, IEND, SINC, ORIGIN,
     *      TERM, LUN, IFIND, IGBLA, IERR)
         END IF
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COLATE: ERROR OPENING OUTPUT TEXT FILE, IERR=', I3)
      END
      SUBROUTINE EXTPAR (RPARM, XINC, YINC, ISTART, IEND, SINC, ORIGIN,
     *   OPCODE, TERM, IGBLA, APPEND, IERR)
C-----------------------------------------------------------------------
C     Extract values from header and AIPS inputs
C     Input:
C       RPARM      R       AIPS parameters
C     Output:
C        XINC      R       X-axis increment (arcsec)
C        YINC      R       Y-axis increment (arcsec)
C        ISTART    I       First slice to collate
C        IEND      I       Last slice to collate
C        SINC      R       save a slice point every SINC arcsec
C        ORIGIN    R       x,y pixel coordinates of reference pixel
C        OPCODE    C*4     'MODL' or 'SLVL'
C        TERM      L       .true. if writing to terminal, else disk file
C        IGBLA     L       Ignore blanks ?
C        APPEND    L       Append or overwrite existing text file
C        IERR      I       0 => OK
C-----------------------------------------------------------------------
      REAL      RPARM(*), XINC, YINC, ORIGIN(4), SINC
      CHARACTER OPCODE*4
      INTEGER   SLMAX, IERR, ISTART, IEND
      LOGICAL   APPEND, IGBLA, TERM
C-----------------------------------------------------------------------
C                                           extract values from header
      CALL EXTHED (XINC, YINC, SLMAX, IERR)
C                                           extract AIPS inputs
      IF (IERR.EQ.0) CALL EXTINP (RPARM, SLMAX, ISTART, IEND, SINC,
     *   ORIGIN, OPCODE, TERM, IGBLA, APPEND, IERR)
C
 999  RETURN
      END
      SUBROUTINE EXTHED (XINC, YINC, SLMAX, IERR)
C-----------------------------------------------------------------------
C     Extract some parameters from the header and change the slice
C     start and end numbers from user given values if necessary
C     Output:
C        XINC      R       X-axis increment (arcsec)
C        YINC      R       Y-axis increment (arcsec)
C        SLMAX     I       maximum number of slices for image
C        IERR      I       0 => OK, else no slices
C-----------------------------------------------------------------------
      REAL      XINC, YINC, CATR(256)
      INTEGER   IERR, SLMAX
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ CATR
C-----------------------------------------------------------------------
C                                          work out increments in arcsec
      XINC = CATR(KRCIC) * 3600.0
      YINC = CATR(KRCIC+1) * 3600.0
C                                          make sure there are slices
      CALL FNDEXT ('SL', CATR, SLMAX)
C                                          double check by looking at
C                                          number of slice files
      IF (SLMAX.LE.0) THEN
         MSGTXT = 'EXTHED: THERE ARE NO SLICES ASSOCIATED WITH THIS'
     *      // ' IMAGE'
         CALL MSGWRT (8)
         IERR = 1
      ELSE
         IERR = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE EXTINP (RPARM, SLMAX, ISTART, IEND, SINC, ORIGIN,
     *   OPCODE, TERM, IGBLA, APPEND, IERR)
C-----------------------------------------------------------------------
C     Extract parameters from AIPS inputs
C     Input:
C       RPARM    R       AIPS inputs
C       SLMAX    I       Maximum number of slices for image
C     Output:
C       ISTART   I       First slice to collate
C       IEND     I       Last slice to collate
C       SINC     R       save a slice point every SINC arcsec
C       ORIGIN   R       x,y pixel coordinates of reference pixel
C       OPCODE   C*4     'MODL' or 'SLVL'
C       TERM     L       If .true. send output to terminal rather than
C                        disk file.
C       IGBLA    L       Ignore blanks ?
C       APPEND   L       Overwrite or append existing text file
C       IERR     I       0 => OK
C
C AIPS parameters
C
C 1      USERID (1)      user no.
C 2-4    INNAME (3)      image name
C 5-6    INCLASS (2)     image class
C 7      INSEQ (1)       image sequence number
C 8      INDISK (1)      image disk number
C 9-20   OUTFILE (12)    output file name
C 21     INVERS (1)      start slice number
C 22     NFILES (1)      number of slices
C 23     ZINC (1)        save a slice point every ZINC arcsec
C 24-30  PIXXY (7)       x-y coordinate of reference pixel
C 31     OPCODE (1)      'MODL', AND 'SLVL'
C 32     APARM(1)        <=0 => don't output blank slice points ('SLVL'
C                        only)
C 33     APARM(2)        <=0 => Overwrite any existing text file
C                        >0  => Append to any existing text file
C-----------------------------------------------------------------------
      CHARACTER SLVL*4, MODL*4, OPCODE*4, CHTMP*4
      REAL      RPARM(*), ORIGIN(4), SINC, CATR(256)
      INTEGER   SLMAX, ISTART, IEND, NSLICE, IERR, CATBLK(256),
     *   IX, IY
      LOGICAL   IGBLA, TERM, APPEND
      EQUIVALENCE (CATBLK, CATR)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ CATBLK
      DATA SLVL, MODL /'SLVL', 'MODL'/
C-----------------------------------------------------------------------
C                                          Start slice number
      ISTART = RPARM(21) + 0.01
C                                          Take all slices
      IF (ISTART.EQ.0) THEN
         ISTART = 1
         IEND = SLMAX
         WRITE (MSGTXT,1000)
         CALL MSGWRT (4)
C                                          Number of slices to collate
      ELSE
         NSLICE = RPARM(22) + 0.01
         IF (NSLICE.LE.0) NSLICE = 1
         IF (ISTART.LT.0) ISTART = 1
C                                          Make sure start slice exists
         IF (ISTART.GT.SLMAX) THEN
            WRITE (MSGTXT,2000) ISTART
            CALL MSGWRT (7)
            IERR = 1
            GO TO 990
            END IF
C                                          End slice number
         IEND = ISTART + NSLICE - 1
         IF (IEND.GT.SLMAX) THEN
            WRITE (MSGTXT,3000)
            CALL MSGWRT (4)
            IEND = SLMAX
            END IF
C                                          Tell user which slices used
         IF (ISTART.EQ.IEND) THEN
            WRITE (MSGTXT,4000) ISTART
         ELSE
            WRITE (MSGTXT,5000) ISTART, IEND
            END IF
         CALL MSGWRT (4)
         END IF
C                                          Slice point increment, 0=>all
      SINC = RPARM(23)
      IF (SINC.LT.0.0) SINC = 0.0
C                                          Reference position, allow it
C                                          off image, but warn user
      ORIGIN(1) = RPARM(24)
      ORIGIN(2) = RPARM(25)
      IF ((ORIGIN(1).EQ.0.0) .AND. (ORIGIN(2).EQ.0.0)) THEN
         ORIGIN(1) = CATR(KRCRP)
         ORIGIN(2) = CATR(KRCRP+1)
         END IF
      ORIGIN(3) = CATR(KRCRP)
      ORIGIN(4) = CATR(KRCRP+1)
      IX = CATBLK(KINAX)
      IY = CATBLK(KINAX+1)
      IF (ORIGIN(1).LT.1 .OR. ORIGIN(2).LT.1 .OR.
     *    ORIGIN(1).GT.IX .OR. ORIGIN(2).GT.IY) THEN
         WRITE (MSGTXT,6000)
         CALL MSGWRT (8)
         END IF
      WRITE (MSGTXT,6010) ORIGIN(1), ORIGIN(2)
      CALL MSGWRT (4)
      WRITE (MSGTXT,6011) ORIGIN(3), ORIGIN(4)
      CALL MSGWRT (4)
C                                          Make sure opcode OK
      IERR = 0
      CALL H2CHR (4, 1, RPARM(31), CHTMP)
      IF (CHTMP.EQ.MODL .OR. CHTMP.EQ.SLVL) THEN
         OPCODE = CHTMP
      ELSE IF (CHTMP.EQ.'    ') THEN
         OPCODE = SLVL
      ELSE
         WRITE (MSGTXT,7000)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 990
         END IF
C                                          Output to disk or terminal ?
      CALL H2CHR (4, 1, RPARM(9), CHTMP)
      IF (CHTMP.EQ.'    ') THEN
         TERM = .TRUE.
      ELSE
         TERM = .FALSE.
         END IF
C                                          Ignore blanks in slice ?
      IF (RPARM(32).LE.0.0) THEN
         IGBLA = .TRUE.
      ELSE
         IGBLA = .FALSE.
         END IF
C                                          Append or overwrite
      IF (RPARM(33).GT.0.0) THEN
         APPEND = .FALSE.
      ELSE
         APPEND = .TRUE.
         END IF
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTINP: Selected all available slices')
 2000 FORMAT ('EXTINP: Start slice = ', I3, ' > actual number of ',
     *        'existing slices')
 3000 FORMAT ('EXTINP: reducing end slice number to maximum available')
 4000 FORMAT ('EXTINP: selected slice number ', I3)
 5000 FORMAT ('EXTINP: selected slice numbers ', I3, ' TO ', I3)
 6000 FORMAT ('EXTINP: WARNING, REFERENCE POSITION OFF IMAGE')
 6010 FORMAT ('Pixel origin X ',F9.2,'  Y',F9.2)
 6011 FORMAT ('Header origin X',F9.2,'  Y',F9.2)
 7000 FORMAT ('EXTINP: UNRECOGNIZED OPCODE')
      END
      SUBROUTINE DOMODL (SLOT, VOL, XINC, YINC, ISTART, IEND, ORIGIN,
     *   TERM, LUN, IFIND, IERR)
C-----------------------------------------------------------------------
C   Main subroutine for the 'MODL' opcode.
C   Input:
C      SLOT     I        Cat slot of associated image
C      VOL      I        Volume no. of associated image
C      XINC     R        x-axis increment (arcsec)
C      YINC     R        y-axis increment (arcsec)
C      ISTART   I        First slice to collate
C      IEND     I        Last slice to collate
C      ORIGIN   R        User suplied reference pixel
C      TERM     L        .true. => output to terminal, else disk file
C      LUN      I        LUN for disk file
C      IFIND    I        Index in FTAB for LUN
C   Output:
C      IERR     I        0 => OK
C-----------------------------------------------------------------------
      REAL   XINC, YINC, ORIGIN(4), SBLKR(256), BLC(2), TRC(2),
     *   PEAK(4), POS(3,4), FWHM(4)
      INTEGER   SLOT, VOL, ISTART, IEND, LUN, IERR, LUNSL, ISFIND,
     *   SBLKI(256), NMODS, NPTS, I, IFIND, MODREC, NGAUSS, J, K
      LOGICAL   TERM, WAIT, EXCL
      CHARACTER LINE*72
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (SBLKI, SBLKR)
      DATA LUNSL /27/
      DATA WAIT, EXCL /.TRUE., .TRUE./
C-----------------------------------------------------------------------
C                                           write output header
      IF (TERM) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
      ELSE
         WRITE (LINE,1000)
         CALL ZTXIO ('WRIT', LUN, IFIND, LINE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
C                                           loop over slices
      DO 100 I = ISTART,IEND
C                                           open slice
         CALL OPEXT ('SL', VOL, SLOT, I, LUNSL, EXCL, WAIT, ISFIND,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,3000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                           read first info. record
         CALL ZFIO ('READ', LUNSL, ISFIND, 1, SBLKI, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,4000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                           get data for last model
         NMODS = SBLKI(58)
         NPTS  = SBLKI(57)
         MODREC = SBLKI(59) - 1
C                                           read second info. record
         IF (NMODS.GT.0) THEN
            CALL ZFIO ('READ', LUNSL, ISFIND, 2, SBLKI, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,5000) I, IERR
               CALL MSGWRT (8)
               GO TO 990
               END IF
C                                       BLC and TRC of slice
            BLC(1) = SBLKR(19)
            BLC(2) = SBLKR(20)
            TRC(1) = SBLKR(26)
            TRC(2) = SBLKR(27)
C                                       read model data
            DO 60 K = 1,NMODS
               MODREC = MODREC + 1
               CALL ZFIO ('READ', LUNSL, ISFIND, MODREC, SBLKI, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,6000) I, MODREC, IERR
                  CALL MSGWRT (8)
                  GO TO 990
                  END IF
               NGAUSS = SBLKI(27)
C                                           work out model parameters
               CALL MODPAR (NGAUSS, BLC, TRC, XINC, YINC, ORIGIN, SBLKR,
     *            NPTS, PEAK, POS, FWHM)
C                                           write output model
               DO 50 J = 1,NGAUSS
                  WRITE (MSGTXT,8000) I, K, J, POS(1,J), POS(2,J),
     *               POS(3,J), PEAK(J), FWHM(J)
                  IF (TERM) THEN
                     CALL MSGWRT (4)
                  ELSE
                     LINE = MSGTXT
                     CALL ZTXIO ('WRIT', LUN, IFIND, LINE, IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,9000) I, IERR
                        CALL MSGWRT (8)
                        GO TO 990
                        END IF
                     END IF
 50               CONTINUE
 60            CONTINUE
            END IF
C                                           close slice file
         CALL ZCLOSE (LUNSL, ISFIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,10000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
 100     CONTINUE
C                                           close text file ?
      IF (.NOT.TERM) THEN
         CALL ZTXCLS (LUN, IFIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,11000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('isl m n  Xpos ''''',4X,'Ypos ''''',4X,'Dist ''''',4X,
     *   'Peak',7X,'FWHM ''''')
 2000 FORMAT ('DOMODL: ERROR WRITING HEADER TO OUTPUT FILE, IERR=', I3)
 3000 FORMAT ('DOMODL: ERROR OPENING SLICE FILE #', I3, ' IERR=', I3)
 4000 FORMAT ('DOMODL: ERROR READING REC. 1 OF SLICE #', I3,
     *        ' IERR=', I3)
 5000 FORMAT ('DOMODL: ERROR READING REC. 2 OF SLICE #', I3,
     *        ' IERR=', I3)
 6000 FORMAT ('DOMODL: ERROR READING SLICE RECORD #', I3, ' MODEL #',
     *        I3, ' IERR=', I3)
 7000 FORMAT ('DOMODL: WARNING, THERE ARE NO MODELS FOR SLICE #', I3)
 8000 FORMAT (I3,2I2,5(1PE11.3))
 9000 FORMAT ('DOMODL: ERROR WRITING SLICE #', I2, ' MODEL TO FILE,',
     *        ' IERR = ', I3)
10000 FORMAT ('DOMODL: ERROR CLOSING SLICE #', I3, ' IERR=', I3)
11000 FORMAT ('DOMODL: ERROR CLOSING OUTPUT TEXT FILE, IERR=', I3)
      END
      SUBROUTINE MODPAR (NGAUSS, BLC, TRC, XINC, YINC, ORIGIN, DATA,
     *   NPTS, PEAK, POS, FWHM)
C-----------------------------------------------------------------------
C   Work out model parameters
C   Inputs:
C      BLC       R       BLC of slice
C      TRC       R       TRC of slice
C      XINC      R       x-axis increment (arcsec)
C      YINC      R       y-axis increment (arcsec)
C      DATA      R       Model data
C      NPTS      I       Number of points in slice
C   Output:
C      PEAK      R       Model peak (disk units)
C      POS       R       Position of peak (arcsec) from slice centre
C      FWHM      R       FWHM (arcsec)
C-----------------------------------------------------------------------
      INTEGER   NGAUSS, NPTS
      REAL      BLC(2), TRC(2), XINC, YINC, ORIGIN(4), DATA(12),
     *   PEAK(4), POS(3,4), FWHM(4)
C
      REAL      WIDRAT, XSEC, YSEC, SLEN, FLOPTS
      INTEGER   I, J
C-----------------------------------------------------------------------
C                                           length of slice in arcsec
      XSEC = (TRC(1) - BLC(1)) * XINC
      YSEC = (TRC(2) - BLC(2)) * YINC
      SLEN = SQRT (XSEC**2.0 + YSEC**2.0)
      FLOPTS = REAL (NPTS - 1)
C                                       loop over Gaussians
      DO 20 I = 1,NGAUSS
         J = (I-1) * 3 + 1
         PEAK(I) = DATA(J)
         XSEC = BLC(1) + (TRC(1)-BLC(1)) * (DATA(J+1)-1) / FLOPTS
         YSEC = BLC(2) + (TRC(2)-BLC(2)) * (DATA(J+1)-1) / FLOPTS
         POS(1,I) = (XSEC - ORIGIN(1)) * XINC
         POS(2,I) = (YSEC - ORIGIN(2)) * YINC
         POS(3,I) = SQRT (POS(1,I)**2 + POS(2,I)**2)
         IF (TRC(1).EQ.BLC(1)) THEN
            IF (POS(2,I).LT.0.) POS(3,I) = -POS(3,I)
         ELSE
            IF (POS(1,I).LT.0.) POS(3,I) = -POS(3,I)
            END IF
         WIDRAT = DATA(J+2) / FLOPTS
         FWHM(I) = WIDRAT * SLEN
         POS(1,I) = (XSEC - ORIGIN(3)) * XINC
         POS(2,I) = (YSEC - ORIGIN(4)) * YINC
 20      CONTINUE
C
      RETURN
      END
      SUBROUTINE DOSLVL (SLOT, VOL, XINC, YINC, ISTART, IEND, SINC,
     *   ORIGIN, TERM, LUN, IFIND, IGBLA, IERR)
C-----------------------------------------------------------------------
C   Main subroutine for the 'SLVL' opcode.
C   Input:
C      SLOT     I        Cat slot of associated image
C      VOL      I        Volume no. of associated image
C      XINC     R        x-axis increment (arcsec)
C      YINC     R        y-axis increment (arcsec)
C      ISTART   I        First slice to collate
C      IEND     I        Last slice to collate
C      SINC     R        save a slice point every SINC arcsec
C      ORIGIN   R        User suplied reference pixel
C      TERM     L        .true. => output to terminal, else disk file
C      LUN      I        LUN for output file
C      IFIND    I        Index of LUN in FTAB
C      IGBLA    L        Ignore blanks ?
C   Output:
C      IERR     I        0 => OK
C-----------------------------------------------------------------------
      REAL      XINC, YINC, ORIGIN(4), SBLKR(256), XPSINC, YPSINC,
     *   SIGNX, SIGNY, BLC(2), TRC(2), IMVAL, DIST(3), SINC
      INTEGER   IREC, IFIND
      INTEGER   SLOT, VOL, ISTART, IEND, LUN, IERR, I, NRPBLK,
     *   LUNSL, ISFIND, SBLKI(256), NPTS, ISREC, IEREC, IWORD,
     *   IPTS, INC
      LOGICAL   IGBLA, TERM, WAIT, EXCL
      CHARACTER LINE*72
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (SBLKI, SBLKR)
      DATA LUNSL /27/
      DATA WAIT, EXCL /.TRUE., .TRUE./
C-----------------------------------------------------------------------
C                                           loop over slice files
      NRPBLK = 256
      DO 500 I = ISTART,IEND
         WRITE (MSGTXT,800) I
         CALL MSGWRT (4)
C                                           open slice file
         CALL OPEXT ('SL', VOL, SLOT, I, LUNSL, EXCL, WAIT, ISFIND,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                           read first info. record
         CALL ZFIO ('READ', LUNSL, ISFIND, 1, SBLKI, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NPTS = SBLKI(57)
C                                           read second info. record
         CALL ZFIO ('READ', LUNSL, ISFIND, 2, SBLKI, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,3000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                           pixels per slice point
C                                           conversion factors
         CALL CONFAC (SINC, XINC, YINC, SBLKR, NPTS, XPSINC, YPSINC,
     *      SIGNX, SIGNY, BLC, TRC, INC)
C                                           write output header
         IF (TERM) THEN
            WRITE (MSGTXT,3200)
            CALL MSGWRT (4)
         ELSE
            WRITE (LINE,3200)
            CALL ZTXIO ('WRIT', LUN, IFIND, LINE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,3400) I
               CALL MSGWRT (8)
               GO TO 990
               END IF
            END IF
C                                           read in slice data records
         IPTS = 1
         ISREC = 3
         IEREC = ISREC - 1 + (NPTS / NRPBLK)
         DO 400 IREC = ISREC,IEREC
            CALL ZFIO ('READ', LUNSL, ISFIND, IREC, SBLKI, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,4000) I, IREC, IERR
               CALL MSGWRT (8)
               GO TO 990
               END IF
C                                            loop over slice points
C                                            in current record
            DO 300 IWORD = 1, NRPBLK
               IF (MOD(IPTS,INC).EQ.1 .OR. INC.EQ.1) THEN
C                                            work out results
                  IF ((SBLKR(IWORD).NE.FBLANK) .OR. (.NOT.IGBLA)) THEN
                     CALL VALPOS (SBLKR(IWORD), XINC, YINC, BLC, TRC,
     *                  SIGNX, SIGNY, XPSINC, YPSINC, ORIGIN, IPTS,
     *                  DIST, IMVAL)
C                                            write output
                     IF (TERM) THEN
                        WRITE (MSGTXT,6000) IPTS, DIST, IMVAL
                        CALL MSGWRT (4)
                     ELSE
                        WRITE (LINE,6000) IPTS, DIST, IMVAL
                        CALL ZTXIO ('WRIT', LUN, IFIND, LINE, IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,7000) I, IERR
                           CALL MSGWRT (8)
                           GO TO 990
                           END IF
                        END IF
                     END IF
                  END IF
               IPTS = IPTS + 1
 300           CONTINUE
 400        CONTINUE
C                                           close slice file
         CALL ZCLOSE (LUNSL, ISFIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,8000) I, IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
 500     CONTINUE
C                                           close output text file ?
      IF (.NOT.TERM) THEN
         CALL ZTXCLS (LUN, IFIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,9000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C
 990  RETURN
C-----------------------------------------------------------------------
  800 FORMAT ('DOSLVL: Processing SLice number', I4)
 1000 FORMAT ('DOSLVL: ERROR OPENING SLICE FILE #', I3, ' IERR=', I3)
 2000 FORMAT ('DOSLVL: ERROR READING REC. 1 OF SLICE #', I3,
     *        ' IERR=', I3)
 3000 FORMAT ('DOSLVL: ERROR READING REC. 2 OF SLICE #', I3,
     *        ' IERR=', I3)
 3100 FORMAT ('     Distance ''''      Value       Value/beam   No.')
 3200 FORMAT ('Number  X pos ''''',6X,'Y pos ''''',6X,'Distance ''''',
     *   3X,'Value')
 3400 FORMAT ('DOSLVL: ERROR WRITING HEADER INFORMATION TO OUTPUT',
     *        ' FOR SLICE # ', I3)
 4000 FORMAT ('DOSLVL: ERROR READING SLICE #', I3, ' RECORD #', I3,
     *        ' IERR=', I3)
 6000 FORMAT (I5,4(1PE14.5))
 7000 FORMAT ('DOSLVL: ERROR WRITING SLICE #', I2, ' TO OUTPUT FILE',
     *        ' IERR=', I3)
 8000 FORMAT ('DOSLVL: ERROR CLOSING SLICE #', I3, ' IERR=', I3)
 9000 FORMAT ('DOSLVL: ERROR CLOSING OUTPUT TEXT FILE, IERR=', I3)
      END
      SUBROUTINE CONFAC (SINC, XINC, YINC, SBLKR, NPTS, XPSINC, YPSINC,
     *   SIGNX, SIGNY, BLC, TRC, INC)
C-----------------------------------------------------------------------
C     Work out slope of slice (in pixels), no. pixels per slice point
C     and some direction signs.
C     Input:
C      SINC     R        save a slice point every SINC arcsec
C      XINC     R        x axis increment arcsec/pixel
C      YINC     R        y axis increment arcsec/pixel
C      SBLKR    R        Slice information records
C      NPTS     I        Number of points in slice
C     Output:
C      XPSINC   R        x increment (pixels) when step along slice by
C                        one point.
C      YPSINC   R        y increment (pixels) when step along slice by
C                        one point.
C      SIGNX    R        x direction sign, = +1.0 if slice from left to
C                        right
C      SIGNY    R        y direction sign, = +1.0 if slice from bottom
C                        to top
C      BLC      R        Slice BLC
C      TRC      R        Slice TRC
C      INC      I        Save a slice point every INC slice points
C-----------------------------------------------------------------------
      REAL   SBLKR(*), XPSINC, YPSINC, PSINC, SLOPE, YINT, BLC(2),
     *   TRC(2), XDIST, YDIST, DIST, SIGNX, SIGNY, SLOPSQ, PSINSQ, SINC,
     *   XINC, YINC, XDISQ, YDISQ, AXDISQ, AYDISQ, ADIST, RAT
      INTEGER   NPTS, INC
      LOGICAL   VERT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                           direction signs
      BLC(1) = SBLKR(19)
      BLC(2) = SBLKR(20)
      TRC(1) = SBLKR(26)
      TRC(2) = SBLKR(27)
      IF (TRC(1).GE.BLC(1)) THEN
         SIGNX = 1.0
         IF (TRC(2).GE.BLC(2)) THEN
            SIGNY = 1.0
         ELSE
            SIGNY = -1.0
            END IF
      ELSE
         SIGNX = -1.0
         IF (TRC(2).GE.BLC(2)) THEN
            SIGNY = 1.0
         ELSE
            SIGNY = -1.0
            END IF
         END IF
C                                           find equation of slice
      YDIST = TRC(2) - BLC(2)
      XDIST = TRC(1) - BLC(1)
      XDISQ = XDIST * XDIST
      YDISQ = YDIST * YDIST
C
      IF (XDIST.NE.0.0) THEN
         SLOPE = YDIST / XDIST
         YINT = BLC(2) - (SLOPE * BLC(1))
         VERT = .FALSE.
      ELSE
         VERT = .TRUE.
         END IF
C                                           pixels / slice points
C                                           x and y conversion factors
      IF (.NOT.VERT) THEN
         SLOPSQ = SLOPE * SLOPE
         DIST = SQRT (XDISQ + YDISQ)
         PSINC = DIST / REAL (NPTS - 1)
         PSINSQ = PSINC * PSINC
         XPSINC = SQRT( PSINSQ / (1.0 + SLOPSQ) )
         YPSINC = ABS (SLOPE) * XPSINC
      ELSE
         DIST = ABS (YDIST)
         XPSINC = 0.0
         YPSINC = DIST / REAL (NPTS - 1)
         END IF
C     ENCODE (80, 1000, MSGTXT) XPSINC
C     CALL MSGWRT (8)
C     ENCODE (80, 2000, MSGTXT) YPSINC
C     CALL MSGWRT (8)
C                                           slice point increment
      AXDISQ = XINC * XINC * XDISQ
      AYDISQ = YINC * YINC * YDISQ
      ADIST = SQRT (AXDISQ + AYDISQ)
      RAT = NPTS * SINC / ADIST
      IF (RAT.LE.0.0) THEN
         INC = 1
      ELSE IF (RAT.LE.1.0) THEN
         INC = 1
         WRITE (MSGTXT,3000)
         CALL MSGWRT (6)
      ELSE IF (RAT.GT.FLOAT(NPTS)) THEN
         INC = 1
         WRITE (MSGTXT,4000)
         CALL MSGWRT (6)
      ELSE
         INC = RAT + 0.5
         END IF
      WRITE (MSGTXT,5000) INC
      CALL MSGWRT (4)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONFAC: ', 1PE12.5, ' X pixels per SLice point')
 2000 FORMAT ('CONFAC: ', 1PE12.5, ' Y pixels per SLice point')
 3000 FORMAT ('CONFAC: ZINC TOO SMALL')
 4000 FORMAT ('CONFAC: ZINC TOO LARGE')
 5000 FORMAT ('CONFAC: SLice point increment = ', I4)
      END
      SUBROUTINE VALPOS (SLDAT, XINC, YINC, BLC, TRC, SIGNX, SIGNY,
     *   XPSINC, YPSINC, ORIGIN, IPT, DIST, SLVAL)
C-----------------------------------------------------------------------
C   Work out value of current slice point and distance from reference
C   pos
C   Input:
C      SLDAT    R        Unscaled slice value
C      X,YINC   R        x- and y-axis coordiante increments
C                        (arcsec/pixel)
C      BLC      R        Slice BLC
C      TRC      R        Slice TRC
C      SIGNX    R        x direction sign, = +1.0 if slice from left to
C                        right
C      SIGNY    R        y direction sign, = +1.0 if slice from bottom
C                        to top
C      XPSINC   R        x increment (pixels) when step along slice by
C                        one point
C      YPSINC   R        y increment (pixels) when step along slice by
C                        one point
C      ORIGIN   R        Reference pixel coordinate
C      IPT      I        Current slice point number 1 : N
C   Output:
C      DIST     R(3)     X, Y, Distance ('') of slice point from reference
C                        pos.
C      SLVAL    R        Value of slice point
C-----------------------------------------------------------------------
      REAL      SLDAT, BLC(2), TRC(2), SIGNX, SIGNY, XPSINC, YPSINC,
     *   ORIGIN(4), DIST(3), SLVAL, X, Y, XINC, YINC, SLPOS
      INTEGER   IPT
C-----------------------------------------------------------------------
      SLPOS = REAL (IPT - 1)
      X = BLC(1) + (SIGNX * SLPOS * XPSINC)
      Y = BLC(2) + (SIGNY * SLPOS * YPSINC)
      DIST(1) = (X - ORIGIN(1)) * XINC
      DIST(2) = (Y - ORIGIN(2)) * YINC
C
      DIST(3) = SQRT (DIST(1)**2 + DIST(2)**2)
      IF (BLC(1).EQ.TRC(1)) THEN
         IF (DIST(2).LT.0.0) DIST(3) = -DIST(3)
      ELSE
         IF (DIST(1).LT.0.0) DIST(3) = -DIST(3)
         END IF
      DIST(1) = (X - ORIGIN(3)) * XINC
      DIST(2) = (Y - ORIGIN(4)) * YINC
      SLVAL = SLDAT
C
      RETURN
      END

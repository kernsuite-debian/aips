LOCAL INCLUDE 'SLICE.INC'
C                                       Local include for SLICE
      INCLUDE 'INCS:PMAD.INC'
      REAL      PRUSER, SEQIN, DSKIN, BLC(7), TRC(7), KEEP
      HOLLERITH XNAMIN(3), XCLSIN(2), XOFILE(12)
      CHARACTER NAMIN*12, CLSIN*6, OFILE*48
      INTEGER   MORD, MORD1, MORD12, MORD21, LUNPR, PFIND, IDEPTH(5)
      LOGICAL   WFILE, ISFQID
      DOUBLE PRECISION ZZ, FRQS(MAXIMG)
      COMMON /INPARM/ PRUSER, XNAMIN, XCLSIN, SEQIN, DSKIN, BLC, TRC,
     *   XOFILE, KEEP
      COMMON /CHRCOM/ NAMIN, CLSIN, OFILE
      COMMON /STCOM/ FRQS, ZZ, MORD, MORD1, MORD12, MORD21, LUNPR,
     *   PFIND, WFILE, IDEPTH, ISFQID
LOCAL END
LOCAL INCLUDE 'SLICE2.INC'
C                             Everett Interpolation internal variables:
      REAL      BCOEF(49), SV(14), SAVWTS(8,61)
      INTEGER   MORD2, IS0, NVALS
      LOGICAL   LRECUR
      COMMON /CEVI/ BCOEF, SV, SAVWTS, LRECUR, MORD2, IS0, NVALS
LOCAL END
      PROGRAM SLICE
C-----------------------------------------------------------------------
C! Interpolates image values along a slice and writes an SL file.
C# Map EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005, 2009, 2011-2012, 2015-2016
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
C   SLICE will create an extension file of type SL containing a set
C   of interpolated pixel values on the line between a given BLC and
C   TRC.  These values are stored as floating pt numbers.  Fractional
C   BLC and TRCs are allowed.  "Almost standard" extension file
C   header format is used.
C   Inputs:   (from AIPS)
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R   sequence number of primary file.
C              INDISK   R   disk volume number. 0 means try all.
C              BLC      R(7)   the coordinate in the input file to
C                       become the left hand coordinate of the slice.
C                       BLC(1) is the X coordinate and
C                       BLC(2) is the Y coordinate.  The first
C                       coordinate in the input image is (1,1).
C              TRC      R(7)   the coordinate in the input file to
C                       become the top right hand corner of the
C                       slice.
C              OUTFILE  H(12) File name to write slice output.
C                       Blank for default (no file written).
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IOBLK(MAXIMG), IMSTUF(37), ISSTUF(6), IMLUN, ISLUN,
     *   IBSIZE, INOSL, IERR, IORD, I0, I1
      LOGICAL   TWODIM
      INCLUDE 'SLICE.INC'
      INCLUDE 'SLICE2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IMLUN, ISLUN /16, 27/
      DATA IBSIZE /8192/
C-----------------------------------------------------------------------
C                                       Order of interpolation.
      IORD = 3
C                                       Get parms, open map, create sli
      CALL SLIINI (IORD, IMLUN, ISLUN, IBSIZE, I0, I1, INOSL, TWODIM,
     *   IMSTUF, ISSTUF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Call 2D or 3D slice subroutine.
      IF (TWODIM) THEN
         CALL SLTTWO (INOSL, BLC, TRC, IMSTUF, ISSTUF, I0, I1, IOBLK,
     *      IERR)
      ELSE
         CALL SLTHRE (INOSL, BLC, IMSTUF, ISSTUF, IOBLK, IERR)
         END IF
C                                       Shutdown
 900  CALL DIE (IERR, IOBLK)
C
 999  STOP
      END
      SUBROUTINE SLIINI (IORD, IMLUN, ISLUN, IBSIZE, I0, I1, INOSL,
     *   TWODIM, IMSTUF, ISSTUF, IERR)
C-----------------------------------------------------------------------
C   This routine does some of the initial processing for the slice
C   file creation routine.  Get the input parameters, resume AIPS if
C   RQUICK, set up the IO and header commons, open the map file and
C   create and open the slice file.
C   Inputs:
C      IORD   I       Order of the interpolation.
C      IMLUN  I       LUN to use for image file.
C      ISLUN  I       LUN to use for slice file.
C      IBSIZE I       Size of image IO buffer in INTEGER*2.
C   Outputs:
C      I0     I       Number of values per row in map window needed
C                     for interpolation.
C      I1     I       Number of rows needed at one time to interpolate
C                     one point.
C      INOSL  I       Number of points in slice.
C      TWODIM L       T if slice two dimensional, else F
C      IMSTUF I(37)   IO LUNs, counters, etc. for image file.
C      ISSTUF I(6)    LUN, FIND, ., ., ., vers # for slice file.
C      IERR   I       Error code, 0=ok.
C      OFILE  C(48)   File name for output file.
C-----------------------------------------------------------------------
      INTEGER   IORD, IMLUN, ISLUN, IBSIZE, I0, I1, INOSL, IMSTUF(37),
     *   ISSTUF(6), IERR
      LOGICAL   TWODIM
C
      CHARACTER NAME*36, TYPIN*2, PRGNAM*6, CTEMP*8
      HOLLERITH MAP
      INTEGER   NPARMS, I, IBLCX, IBLCY, ITRCX, ITRCY, NX, NY, INODIM,
     *   NFQ, ORDER, NZI, J, J1, J2
      REAL      SVX(14), RPARMS(34)
      DOUBLE PRECISION DTEMP
      INCLUDE 'SLICE.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (PRUSER, RPARMS)
      DATA PRGNAM /'SLICE '/
C-----------------------------------------------------------------------
C                                       Get parms, set IO & hdr vals.
      NPARMS = 34
      CALL TSKBEG (PRGNAM, NPARMS, XNAMIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (48, 1, XOFILE, OFILE)
      TYPIN = 'MA'
      PRUSER = NLUSER
      NPARMS = NPARMS + 1
C                                       Open the map file.
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (XNAMIN, XCLSIN, SEQIN, MAP, DSKIN, PRUSER, NAME)
C                                       Open input map
      CALL OPENCF (IMLUN, NAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INPUT IMAGE'
         GO TO 990
         END IF
      IMSTUF(1) = FILTAB(POLUN,6)
      IMSTUF(2) = FILTAB(POFIN,6)
      IMSTUF(5) = FILTAB(POCAT,6)
      IMSTUF(7) = FILTAB(POVOL,6)
C                                       Set up info used by DIE.
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IMSTUF(7)
      FCNO(NCFILE) = IMSTUF(5)
      FRW(NCFILE) = 0
C                                       Get header
      CALL GETHDR (IMLUN, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING HEADER'
         GO TO 990
         END IF
C                                       set windows
      CALL SLNDOW (FILTAB(PODIM,6), FILTAB(PONAX,6), KEEP, BLC, TRC,
     *   IERR)
      IF (IERR.EQ.0) THEN
         DO 10 I = 1,7
            FILTAB(POBLC+I-1,6) = BLC(I) + 0.001
            FILTAB(POTRC+I-1,6) = TRC(I) + 0.999
 10         CONTINUE
         FILTAB(POIOP,6) = -ABS (FILTAB(POIOP,6))
         CALL COPY (6, FILTAB(POBLC+1,6), FILTAB(PODEP,6))
C                                       Compute Block offset to start
C                                       of windowed region
         CALL COMOFF (FILTAB(PODIM,6), FILTAB(PONAX,6),
     *      FILTAB(POBLC+2,6), FILTAB(POBL,6), IERR)
         FILTAB(POBL,6) = 1 + FILTAB(POBL,6)
      ELSE
         WRITE (MSGTXT,1000) IERR, 'SETTING WINDOWS'
         GO TO 990
         END IF
C                                       See if two dim slice.
      INODIM = CATBLK(KIDIM)
      CTEMP = ' '
      NZI = 0
      J = 0
      DO 30 I = 1,INODIM
         IF (I.GT.2) IDEPTH(I-2) = BLC(I) + 0.01
         IF (ABS(TRC(I)-BLC(I)).GT.0.1) THEN
            IF (I.GT.2) J = J + 1
            NZI = I
            END IF
 30      CONTINUE
      TWODIM = NZI.LE.2
      NX = ABS (TRC(1) - BLC(1)) + 0.01
      NY = ABS (TRC(2) - BLC(2)) + 0.01
C                                       re-open w special I/O
      IF (TWODIM) THEN
         CALL FILCLS (IMLUN)
         NCFILE = NCFILE - 1
         CALL INTMIO (IMLUN, 'WRIT', NAME, BLC, TRC, IBSIZE, CATBLK,
     *      IMSTUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'RE-OPENING INPUT IMAGE'
            GO TO 990
            END IF
         IF ((NX.GT.0) .AND. (NY.GT.0)) KEEP = -1.0
C                                       3D - allow only simple slice
      ELSE
         IF ((NX.GT.0) .OR. (NY.GT.0)) THEN
            IERR = 1
            MSGTXT = 'CANNOT DO TRUE 3+ DIMENSIONS'
            GO TO 990
         ELSE IF (J.GT.1) THEN
            IERR = 1
            MSGTXT = 'CANNOT SLICE IN > 1 DIMENSION ABOVE X,Y'
            GO TO 990
            END IF
         END IF
C                                       FQID problem?
      CALL H2CHR (8, 1, CATH(KHCTP+2*(NZI-1)), CTEMP)
      ISFQID = CTEMP.EQ.'FQID'
C                                       init coordinates
      LOCNUM = 1
      CALL SETLOC (IDEPTH, .FALSE.)
      IF (AXTYP(LOCNUM).LE.1) ZZ = CATD(KDCRV+2) + CATR(KRCIC+2) *
     *   (IDEPTH(1) - CATR(KRCRP+2))
C                                       Set initial values for interpo
      CALL INITEI (IORD)
C                                       Adjust windows to get neighbor
C                                       pixels needed for interpolation.
      CALL IEVERT (BLC(1), IBLCX, SVX)
      CALL IEVERT (BLC(2), IBLCY, SVX)
      CALL IEVERT (TRC(1), ITRCX, SVX)
      CALL IEVERT (TRC(2), ITRCY, SVX)
C                                       Left to right.
      IF (TRC(1)-BLC(1).GT.-0.5) THEN
         IMSTUF(17) = MAX (IBLCX + 1, 1)
         IMSTUF(18) = MIN (ITRCX + MORD12, IMSTUF(10))
C                                       Right to left
      ELSE
         IMSTUF(17) = MAX (ITRCX + 1, 1)
         IMSTUF(18) = MIN (IBLCX + MORD12, IMSTUF(10))
         END IF
C                                       Reading file forward.
      IF (BLC(2).LE.TRC(2)) THEN
         IMSTUF(19) = MAX (IBLCY + 1, 1)
         IMSTUF(20) = MIN (ITRCY + MORD12, IMSTUF(11) )
C                                       Reading file backward.
      ELSE
         IMSTUF(19) = MIN (IBLCY + MORD12, IMSTUF(11) )
         IMSTUF(20) = MAX (ITRCY + 1, 1)
         END IF
C
      IMSTUF(31) = IMSTUF(20)
      I0 = IMSTUF(18) - IMSTUF(17) + 1
      I1 = MORD12
      IMSTUF(9) = I0
C                                       FQID values
      IF (ISFQID) THEN
         CALL FRQGET (IMSTUF(7), IMSTUF(5), NFQ, ORDER, FRQS, IERR)
         IF (ABS(ORDER).NE.1) THEN
            MSGTXT = 'FQID FREQUENCIES NOT IN ORDER'
            CALL MSGWRT (6)
            ISFQID = .FALSE.
            END IF
         END IF
C                                       apply BLC/TRC to FQID freqs
      IF (ISFQID) THEN
         J1 = MIN (BLC(NZI), TRC(NZI)) + 0.1
         J2 = MAX (BLC(NZI), TRC(NZI)) + 0.1
         IF (J1.GT.1) THEN
            I = 0
            DO 50 J = J1,J2
               I = I + 1
               FRQS(I) = FRQS(J)
 50            CONTINUE
            END IF
C                                       reverse order
         IF (BLC(NZI).GT.TRC(NZI)) THEN
            J2 = J2 - J1 + 1
            J1 = J2/2
            DO 60 J = 1,J1
               DTEMP = FRQS(J2-J+1)
               FRQS(J2-J+1) = FRQS(J)
               FRQS(J) = DTEMP
 60            CONTINUE
            END IF
         END IF
C                                       Create and open slice file.
      ISSTUF(1) = ISLUN
      CALL MAKSLI (PRGNAM, RPARMS, NZI, IMSTUF, ISSTUF, CATBLK, INOSL,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING THE SLICE FILE'
         GO TO 990
         END IF
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SLIINI ERROR',I4,' ON ',A)
      END
      SUBROUTINE MAKSLI (PRGNAM, FIRST, NZI, IMSTUF, ISSTUF, CATBLK,
     *   INOSL, IERR)
C-----------------------------------------------------------------------
C   This routine will create, open and initialize a slice file, update
C   the header for the map, and set up an array containing values needed
C   to access a map.
C   Inputs:
C      PRGNAM  C*6      Program name.  This is put in SL file header.
C      FIRST   R        First parameter in common block containing
C                       inputs from AIPS.
C      NZI     I        Relevant axis number if > 2
C      IMSTUF  I(37)    IO info for map file.  Volume all that's used.
C   In/Out:
C      ISSTUF  I(6)     IO pointers for slice file.  Input 1-LUN.
C                       output: 2-FIND, 3-?, 6-version number,
C                       4- last element updated in io buffer (0).
C                       5 - The block in slice file to write current
C                       IO buffer.
C      CATBLK    I(256)   The map header. Will be updated to show slice
C   Outputs:
C      INOSL   I        The number of points in slice.
C      IERR    I        Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NZI, IMSTUF(37), ISSTUF(6), CATBLK(256), INOSL, IERR
      REAL      FIRST(*)
C
      CHARACTER SFILE*48, LINE*80
      REAL      X, Y, RWBLK(512)
      INTEGER   ITRIM, IWBLK(512), IER2, IMAX, NRPBLK, NREC, IVER
      HOLLERITH HWBLK(512)
      DOUBLE PRECISION DWBLK(256), FQFINC
      INCLUDE 'SLICE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (IWBLK, HWBLK, RWBLK, DWBLK)
C-----------------------------------------------------------------------
      LUNPR = 10
C                                       Determine number of slice points
      IF (NZI.LE.2) THEN
         X = ABS(TRC(1) - BLC(1)) + 1.0
         Y = ABS(TRC(2) - BLC(2)) + 1.0
         IMAX = SQRT (X*X + Y*Y) + 0.5
      ELSE
         IMAX = ABS (TRC(NZI)-BLC(NZI)) + 1.01
         END IF
      IF (ISFQID) THEN
         INOSL = 4 * IMAX - 3
      ELSE IF ((NZI.GT.2) .OR. (KEEP.GT.0.0)) THEN
         INOSL = IMAX
      ELSE IF (IMAX.GT.2048) THEN
         INOSL = MAX (IMAX, 8192)
      ELSE IF (IMAX.GT.1024) THEN
         INOSL = 4096
      ELSE IF (IMAX.GT.512) THEN
         INOSL = 2048
      ELSE IF (IMAX.GT.256) THEN
         INOSL = 1024
      ELSE
         INOSL = 512
         END IF
C                                       Create slice file.
      NRPBLK = 256
      NREC = (INOSL - 1) / NRPBLK  +  2
      CALL FILL (256, 0, IWBLK)
      CALL FNDEXT ('SL', CATBLK, IVER)
      IVER = IVER + 1
      CALL EXTINI ('WRIT', 'SL', IMSTUF(7), IMSTUF(5), IVER, CATBLK,
     *   ISSTUF(1), ISSTUF(2), NRPBLK, NREC, IWBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Create output text file,
C                                       if needed.
      WFILE = OFILE(1:1) .NE. ' '
      IF (WFILE) THEN
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE(MSGTXT, 1020) IERR
            CALL MSGWRT(7)
            GO TO 999
            END IF
         WRITE (LINE,1045)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:ITRIM(LINE)), IERR)
         END IF
C                                       Finish ISSTUF
      ISSTUF(3) = 0
      ISSTUF(4) = 0
      ISSTUF(5) = 3
      ISSTUF(6) = IVER
C                                       Save parameters in first record
      CALL ZFIO ('READ', ISSTUF(1), ISSTUF(2), 1, IWBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       # of slice pts, # of models,
C                                       1st avail block for next model.
      IWBLK(57) = INOSL
      IWBLK(58) = 0
      IWBLK(59) = IWBLK(1) + 1
      CALL ZFIO ('WRIT', ISSTUF(1), ISSTUF(2), 1, IWBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Put inputs in 2nd record.
      CALL CHR2H (6, PRGNAM, 1, HWBLK(1))
      CALL ZDATE (IWBLK(4))
      CALL ZTIME (IWBLK(7))
      IWBLK(10) = 23
      CALL COPY (22, FIRST, IWBLK(11))
      CALL CHR2H (4, 'AVER', 1, HWBLK(33))
      IF (ISFQID) THEN
         FQFINC = (FRQS(IMAX) - FRQS(1)) / (INOSL-1)
         RWBLK(36) = FQFINC
         DWBLK(19) = FRQS(1)
         END IF
      CALL ZFIO ('WRIT', ISSTUF(1), ISSTUF(2), 2, IWBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Slice file created message.
      WRITE (MSGTXT,1050) IVER
      CALL MSGWRT (5)
      GO TO 999
C                                       destroy the slice
 990  CALL ZCLOSE (ISSTUF(1), ISSTUF(2), IER2)
      CALL ZPHFIL ('SL', IMSTUF(7), IMSTUF(5), IVER, SFILE, IER2)
      CALL ZDESTR (IMSTUF(7), SFILE, IER2)
      CALL DELEXT ('SL', IMSTUF(7), IMSTUF(5), 'WRIT', CATBLK, IWBLK,
     *   IVER, IER2)
      NCFILE = NCFILE - 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('MAKSLI: ERROR ',I3, ' RECEIVED FRO ZTXOPN')
 1040 FORMAT ('COULD NOT BUILD SLICE FILE. ZCREA3 ERR =',I6)
 1045 FORMAT (4X,'Xpix',6X,'Ypix',3X,'Slice value',5X,'X',14X,'Y',14X,
     *   'Z')
 1050 FORMAT ('SLice file version ',I5,' created.')
      END
      SUBROUTINE SLTTWO (INOSL, BBLC, TTRC, IMSTUF, ISSTUF, I0, I1,
     *   IOBLK, IERR)
C-----------------------------------------------------------------------
C   SLTTWO will write to a slice file for a given portion of a map.
C   SLTTWO is a buffer declaring and checking routine and calls
C   SLTWO.
C   Inputs:  INOSL   I   No. of slice pts.
C            BBLC    R(2)    First slice point (X Y).
C            TTRC    R(2)    Last slice point (X Y).
C            IMSTUF  I(37)   IO counters, pointers, etc. for map.
C                            initialized by INTMIO.
C            ISSTUF  I(6)    LUN, FIND, ., ., ., vers #, for slice file
C            I0      I       Row length needed for interpolation.
C            I1      I       Number of rows needed for interpolation.
C   Output:  IOBLK   I(?)    IO buffer big enough to hold at least one
C                            row of the image.
C            IERR    I       Error indicator. 0 = none.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER SFILE*48
      INTEGER   I0, I1
      REAL      ROWS(MAXIMG,9), BBLC(2), TTRC(2)
      INTEGER   INOSL, IMSTUF(37), ISSTUF(6), IOBLK(*), IERR, IER2,
     *   ISCR(256)
      INCLUDE 'SLICE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (ISCR, ROWS)
C-----------------------------------------------------------------------
      IF (I0*I1.GT.9*MAXIMG) THEN
         IERR = 8
         WRITE (MSGTXT,1000) I0, I1
         CALL MSGWRT (8)
C                                       regular & FQID types
      ELSE
         CALL SLTWO (INOSL, BBLC, TTRC, IMSTUF, ISSTUF, ROWS, I0, I1,
     *      IOBLK, IERR)
         END IF
C                                       destroy the slice
      IF (IERR.GT.0) THEN
         CALL ZCLOSE (ISSTUF(1), ISSTUF(2), IER2)
         CALL ZPHFIL ('SL', IMSTUF(7), IMSTUF(5), ISSTUF(6), SFILE,
     *      IER2)
         CALL ZDESTR (IMSTUF(7), SFILE, IER2)
         CALL DELEXT ('SL', IMSTUF(7), IMSTUF(5), 'WRIT', ISCR, IOBLK,
     *      ISSTUF(6), IER2)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT HANDLE X,Y SIZE = ',2I7)
      END
      SUBROUTINE SLTWO (INOSL, BBLC, TTRC, IMSTUF, ISSTUF, ROWS,
     *   I0, I1, IOBLK, IERR)
C-----------------------------------------------------------------------
C   SLTWO will write to a slice file for a given portion of a map.
C   Inputs:  INOSL   I       No. of slice pts.
C            BBLC    R(2)    First slice point (X Y).
C            TTRC    R(2)    Last slice point (X Y).
C            IMSTUF  I(37)   IO counters, pointers, etc. for map.
C                            initialized by INTMIO.
C            ISSTUF  I(6)    LUN, FIND, ., ., ., vers # for slice file
C            ROWS    R(I0,   I1) Buffer holding rows of image needed
C                            for interpolation.
C            I0      I       Row length needed for interpolation.
C            I1      I       Number of rows needed for interpolation.
C   Output:  IOBLK   I(?)    IO buffer big enough to hold at least one
C                            row of the image.
C            IERR    I       Error indicator. 0 = none.
C-----------------------------------------------------------------------
      INTEGER   INOSL, IMSTUF(37), ISSTUF(6), I0, I1, IOBLK(*), IERR
      REAL      BBLC(2), TTRC(2), ROWS(I0,I1)
C
      REAL      DX, DY, X, Y, SBLK(256), MINMAX(2)
      INTEGER   I, ROWPTR(15), ITRIM, LNOSL
      DOUBLE PRECISION FINC, F0
      CHARACTER LINE*80
      INCLUDE 'SLICE.INC'
      REAL      SLROW(MAXIMG)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (ISFQID) THEN
         LNOSL = (INOSL + 3) / 4
      ELSE
         LNOSL = INOSL
         END IF
C                                       X and Y distances per slice pt.
      DX = (TTRC(1) - BBLC(1)) / (LNOSL - 1)
      DY = (TTRC(2) - BBLC(2)) / (LNOSL - 1)
C                                       Set initial values for min max.
      MINMAX(1) = 1.0E30
      MINMAX(2) = - MINMAX(1)
C                                       Set up row pointers to force
C                                       first reads.
      ROWPTR(1) = IMSTUF(17)
      ROWPTR(3) = IMSTUF(18)
      ROWPTR(5) = IMSTUF(37)
C                                       Reading forward
      IF (IMSTUF(19).LE.IMSTUF(20)) THEN
         ROWPTR(2) = IMSTUF(19) - MORD12
         ROWPTR(4) = IMSTUF(19) - 1
C                                       Backward read
      ELSE
         ROWPTR(2) = IMSTUF(19) + 1
         ROWPTR(4) = IMSTUF(19) + MORD12
         END IF
C
      DO 10 I = 6,14
         ROWPTR(I) = I - 5
 10      CONTINUE
C
      DO 30 I = 1,LNOSL
C                                       Calculate X and Y
         X = (I - 1) * DX  +  BBLC(1)
         Y = (I - 1) * DY  +  BBLC(2)
C                                       See if correct row is in core,
C                                       if not read.
         CALL FNDROW (IMSTUF, Y, I0, I1, ROWPTR, IOBLK, ROWS, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Interpolate.
         CALL ETERP (X, Y, I0, I1, ROWPTR, ROWS, SLROW(I))
C                                       Set slice value, write if needed
         IF (.NOT.ISFQID) THEN
            CALL SLPUT (X, Y, ISSTUF, SLROW(I), MINMAX, SBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 30      CONTINUE
C                                       FQID interpolation needed
      IF (ISFQID) THEN
         CALL FQTERP (LNOSL, SLROW, FRQS)
         FINC = (FRQS(LNOSL) - FRQS(1)) / (INOSL - 1)
         F0 = FRQS(1) - FINC
         DO 40 I = 1,INOSL
            F0 = F0 + FINC
            ISSTUF(4) = ISSTUF(4) + 1
            SBLK(ISSTUF(4)) = SLROW(I)
C                                       Write to text file
C                                       Update the max and min.
            IF (SLROW(I).NE.FBLANK) THEN
               MINMAX(1) = MIN (MINMAX(1), SLROW(I))
               MINMAX(2) = MAX (MINMAX(2), SLROW(I))
               IF (WFILE) THEN
                  WRITE(LINE,1010) SLROW(I), F0
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:ITRIM(LINE)),
     *               IERR)
                  IF (IERR.NE.0) GO TO 999
                  END IF
               END IF
C                                       Write this buffer.
            IF (ISSTUF(4).GE.256) THEN
               CALL ZFIO ('WRIT', ISSTUF(1), ISSTUF(2), ISSTUF(5), SBLK,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               ISSTUF(5) = ISSTUF(5) + 1
               ISSTUF(4) = 0
               END IF
 40         CONTINUE
         END IF
C                                       Slice file cleanup, write last
C                                       buffer, etc.
      CALL SLCLS (ISSTUF, MINMAX, SBLK, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (19X,1PE15.6,1PE15.7)
      END
      SUBROUTINE FNDROW (IMSTUF, Y, I0, I1, ROWPTR, IOBLK, ROWS, IERR)
C-----------------------------------------------------------------------
C   This routine will calculate the rows needed in ROWS to interpolate
C   at a given row.  If the correct row is not in ROWS, the
C   routine will read in the next rows, reading over the oldest rows,
C   until the correct rows are available.  The routine uses a rotating
C   buffer concept using information in ROWPTR to keep track of which
C   rows are where.
C   Inputs:
C     IMSTUF    I(4)    Vector containing all the information needed by
C                       the AIPS IO system for the image, i.e.
C                       LUN, FIND, ... This is initialized by INTMIO.
C     Y         R       Y position of point.
C     I0        I       Length of rows of ROWS.
C     I1        I       Number of rows in ROWS.
C  In/Out:
C     ROWPTR    I(15)   BLCX, BLCY, TRCX, TRCY of plane in ROWS, fwd
C                       backward flag (1 or -1)
C                       Elements 6-? are pointers to rows in ROWS
C                       starting with the bottomest.
C     IOBLK     I(?)    IO buffer, big enough for at least 1 row,
C                       two rows if you want double buffering.
C     ROWS      R(I0,   I1)
C-----------------------------------------------------------------------
      INTEGER   I0, I1
      REAL      ROWS(I0,I1)
      REAL      Y
      INTEGER   IMSTUF(37), ROWPTR(15), IOBLK(*), IERR, IY, IYNEED,
     *   IYGOT, I, IOLDST, IUSED, ISAVE, II, ISGN, ICTR
      LOGICAL   EOF, OFFEDG
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IY = Y
      ISGN = ROWPTR(5)
      IF (ISGN.GT.0) IYNEED = IY + (I1 / 2)
      IF (ISGN.LT.0) IYNEED = IY - (I1 / 2) + 1
      IYGOT = ROWPTR(3+ISGN)
C                                       FORTRAN 66 DO WHILE
      DO 500 I = 1,9999
C                                       See if we have enough pts or
C                                       points we need are off map.
         OFFEDG = ISGN*ROWPTR(3-ISGN) .LT. ISGN*IMSTUF(19)
         IF ( ((ISGN*IYNEED).LE.(ISGN*IYGOT)) .AND. (.NOT.OFFEDG))
     *      GO TO 999
C                                       See if at edge of map.
         IYGOT = IYGOT + ISGN
         IF ( (ISGN*IYGOT).GT.(ISGN*IMSTUF(20))) GO TO 999
         IUSED = ROWPTR(4) - ROWPTR(2) + 6
         IOLDST = ROWPTR(6)
         IF (ISGN.LT.0) IOLDST = ROWPTR(IUSED)
         CALL GETROW (IMSTUF, IOBLK, ROWS(1,IOLDST), EOF, IERR)
         IF (EOF) GO TO 980
         IF (IERR.NE.0) GO TO 999
C                                       Update pointers
         ROWPTR(4) = ROWPTR(4) + ISGN
         ROWPTR(2) = ROWPTR(2) + ISGN
C                                       Reading forward.
         IF (ISGN.GE.0) THEN
            ISAVE = ROWPTR(6)
            DO 100 II = 7,IUSED
               ROWPTR(II-1) = ROWPTR(II)
 100           CONTINUE
            ROWPTR(IUSED) = ISAVE
C                                       Reading backward.
         ELSE
            ISAVE = ROWPTR(IUSED)
            ICTR = IUSED - 6
            DO 400 II = 1,ICTR
               ROWPTR(IUSED-II+1) = ROWPTR(IUSED-II)
 400           CONTINUE
            ROWPTR(6) = ISAVE
            END IF
 500     CONTINUE
      IERR = 1
      WRITE (MSGTXT,1500)
      CALL MSGWRT (8)
      GO TO 999
C                                       Premature EOF
 980  CONTINUE
      WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
      IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('FNDROW: FATAL ERROR COUNTING ROWS!')
 1980 FORMAT ('FNDROW: UNEXPECTED END OF FILE ON MAP')
      END
      SUBROUTINE SLTHRE (INOSL, BBLC, IMSTUF, ISSTUF, IOBLK, IERR)
C-----------------------------------------------------------------------
C   SLTHRE will write to a slice file for a given portion of a map
C   One X,Y pixel per plane.
C   Inputs:
C      INOSL    I       No. of slice pts.
C      BBLC     R(7)    First slice point (X Y).
C      IMSTUF   I(37)   IO counters, pointers, etc. for map.
C                       initialized by INTMIO.
C      ISSTUF   I(6)    LUN, FIND, ., ., ., vers # for slice file
C   Output:
C      IOBLK    I(*)    IO buffer big enough to hold at least one row of
C                       the image.
C      IERR     I       Error indicator. 0 = none.
C-----------------------------------------------------------------------
      INTEGER   INOSL, IMSTUF(37), ISSTUF(6), IOBLK(*), IERR
      REAL      BBLC(7)
C
      REAL      X, Y, SBLK(256), MINMAX(2), WT(4), VALS(4)
      INTEGER   I, ITRIM, LNOSL, IER2, NX, NY
      DOUBLE PRECISION FINC, F0
      CHARACTER LINE*80, SFILE*48
      INCLUDE 'SLICE.INC'
      REAL      SLROW(MAXIMG)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IF (ISFQID) THEN
         LNOSL = (INOSL + 3) / 4
      ELSE
         LNOSL = INOSL
         END IF
C                                       interpolate
      NX = FILTAB(POTRC,6) - FILTAB(POBLC,6) + 1
      NY = FILTAB(POTRC+1,6) - FILTAB(POBLC+1,6) + 1
      CALL RFILL (4, 0.0, VALS)
      X = BBLC(1) - FILTAB(POBLC,6)
      Y = BBLC(2) - FILTAB(POBLC+1,6)
      WT(1) = (1.0 - X) * (1.0 - Y)
      WT(2) = X * (1.0 - Y)
      WT(3) = (1.0 - X) * Y
      WT(4) = X * Y
C                                       Set initial values for min max.
      MINMAX(1) = 1.0E30
      MINMAX(2) = - MINMAX(1)
      X = BBLC(1)
      Y = BBLC(2)
C                                       read data
      DO 30 I = 1,LNOSL
         IF (NX*NY.EQ.1) THEN
            CALL MAPIO ('READ', IMSTUF(1), SLROW(I), IERR)
            IF (IERR.NE.0) GO TO 900
         ELSE
            CALL MAPIO ('READ', IMSTUF(1), VALS(1), IERR)
            IF (IERR.NE.0) GO TO 900
            IF (NY.EQ.2) THEN
               CALL MAPIO ('READ', IMSTUF(1), VALS(3), IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            SLROW(I) = WT(1)*VALS(1) + WT(2)*VALS(2) + WT(3)*VALS(3)
     *         + WT(4)*VALS(4)
            END IF
C                                       Set slice value, write if needed
         IF (.NOT.ISFQID) THEN
            CALL SLPUT (X, Y, ISSTUF, SLROW(I), MINMAX, SBLK, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
 30      CONTINUE
C                                       FQID interpolation needed
      IF (ISFQID) THEN
         CALL FQTERP (LNOSL, SLROW, FRQS)
         FINC = (FRQS(LNOSL) - FRQS(1)) / (INOSL - 1)
         F0 = FRQS(1) - FINC
         DO 40 I = 1,INOSL
            F0 = F0 + FINC
            ISSTUF(4) = ISSTUF(4) + 1
            SBLK(ISSTUF(4)) = SLROW(I)
C                                       Write to text file
C                                       Update the max and min.
            IF (SLROW(I).NE.FBLANK) THEN
               MINMAX(1) = MIN (MINMAX(1), SLROW(I))
               MINMAX(2) = MAX (MINMAX(2), SLROW(I))
               IF (WFILE) THEN
                  WRITE(LINE,1010) SLROW(I), F0
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:ITRIM(LINE)),
     *               IERR)
                  IF (IERR.NE.0) GO TO 900
                  END IF
               END IF
C                                       Write this buffer.
            IF (ISSTUF(4).GE.256) THEN
               CALL ZFIO ('WRIT', ISSTUF(1), ISSTUF(2), ISSTUF(5), SBLK,
     *            IERR)
               IF (IERR.NE.0) GO TO 900
               ISSTUF(5) = ISSTUF(5) + 1
               ISSTUF(4) = 0
               END IF
 40         CONTINUE
         END IF
C                                       Slice file cleanup, write last
C                                       buffer, etc.
      CALL SLCLS (ISSTUF, MINMAX, SBLK, IERR)
C                                       destroy the slice
 900  IF (IERR.GT.0) THEN
         CALL ZCLOSE (ISSTUF(1), ISSTUF(2), IER2)
         CALL ZPHFIL ('SL', IMSTUF(7), IMSTUF(5), ISSTUF(6), SFILE,
     *      IER2)
         CALL ZDESTR (IMSTUF(7), SFILE, IER2)
         CALL DELEXT ('SL', IMSTUF(7), IMSTUF(5), 'WRIT', SBLK, IOBLK,
     *      ISSTUF(6), IER2)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (19X,1PE15.6,1PE15.7)
      END
      SUBROUTINE ETERP (DX, DY, I0, I1, ROWPTR, ROWS, OUTVAL)
C-----------------------------------------------------------------------
C  Subroutine to do Everett interpolation.  INITEI should be called once
C  with the order of the interpolation desired to set up variables in
C  common before any subsequent calls to this routine.
C  Inputs:
C    DX      R        X position.
C    DY      R        Y position.
C    I0      I        Number of X pts in ROWS.
C    I1      I        Number of Y pts in ROWS.
C    ROWPTR  I(14)    Array components 1, 2, 3, 4, 5 contain the X BLC,
C                     Y BLC, X TRC, YTRC of the elements contained in
C                     array ROWS and the forward (1) backward (-1)
C                     read flag. Components 6 though (XTRC - XBLC + 6)
C                     contain indices into ROWS ordered from bottom to
C                     top, that is if ROWPTR(6) equals 4, then ROWS(*,4)
C                     contains the bottomest row of the map that can be
C                     found in ROWS.
C    ROWS    R(I0,   I1) Data grid used in interpolation.  Rows are in
C                     the order specified by ROWPTR (see above).
C-----------------------------------------------------------------------
      INTEGER   I0, I1
      REAL      DX, DY, ROWS(I0,I1), SVX(14), SVY(14), OUTVAL, XSUM,
     *   ZIKP, OUTWT, XWT
      INTEGER   IXI, IYI, K, J, JP, JPZI, ROWPTR(14), KP
      INCLUDE 'SLICE.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      OUTVAL = 0.0
      OUTWT = 0.0
      CALL IEVERT (DX, IXI, SVX)
      CALL IEVERT (DY, IYI, SVY)
C                                       Loop on y-interpolation:
      DO 130 J = 1,MORD12
         JP = J + IYI
         IF (JP.LT.ROWPTR(2)) JP = ROWPTR(2) + ROWPTR(2) - JP - 1
         IF (JP.GT.ROWPTR(4)) JP = ROWPTR(4) + ROWPTR(4) - JP + 1
         JPZI = ROWPTR(JP-ROWPTR(2)+6)
         XSUM = 0.0
         XWT = 0.0
C                                       Do an x-interpolation:
         DO 120 K = 1,MORD12
            KP = K + IXI
            IF ((KP.GE.ROWPTR(1)) .AND. (KP.LE.ROWPTR(3))) GO TO 115
               IF ((KP.LT.ROWPTR(1)-MORD1) .OR. (KP.GT.ROWPTR(3)+MORD1))
     *            GO TO 130
                  IF (KP.LT.ROWPTR(1)) KP = ROWPTR(1) + ROWPTR(1) -
     *               KP - 1
                  IF (KP.GT.ROWPTR(3)) KP = ROWPTR(3) + ROWPTR(3) -
     *               KP + 1
 115        ZIKP = ROWS((KP-ROWPTR(1)+1),JPZI)
            IF (ZIKP.NE.FBLANK) THEN
               XSUM = XSUM + (ZIKP * SVX(K))
               XWT = XWT + SVX(K)
               END IF
 120        CONTINUE
         OUTVAL = OUTVAL + (XSUM * SVY(J))
         OUTWT = OUTWT + (XWT * SVY(J))
 130     CONTINUE
C                                       Use well determined points
      IF (OUTWT.LE.0.5) THEN
         OUTVAL = FBLANK
      ELSE
         OUTVAL = OUTVAL / OUTWT
         END IF
C
 999  RETURN
      END
      SUBROUTINE INITEI (IORD)
C-----------------------------------------------------------------------
C   INITEI computes certain quantities which are needed by IEVERT
C   when it computes the actual weights for an interpolation. The
C   result produced by INITEI is in the BCOEF array in the COMMON
C   block, and is based on binomial coefficients computed by BINOM.
C   From the Everett interpolation package originally coded by
C   Larry Goad at KPNO.
C   Input:
C      IORD    I    The order of the interpolation, 0 = linear,
C                   1 = cubic, 2 = quintic, 3 = septic.
C-----------------------------------------------------------------------
      INTEGER   IORD
      INTEGER   IN, NT, M, N
      REAL      XT
      INCLUDE 'SLICE.INC'
      INCLUDE 'SLICE2.INC'
C-----------------------------------------------------------------------
      MORD = IORD
      MORD1 = MORD + 1
      MORD21 = MORD + MORD1
      MORD12 = MORD1 + MORD1
      MORD2 = MIN (IORD, 6)
      IN = 1
      NT = 0
      BCOEF(1) = 1.
C                                   Compute the BCOEF array:
      DO 50 M = 1,MORD2
         IN = IN + NT + 1
         NT = M + M
         XT = NT
         CALL BINOM (XT, NT, BCOEF(IN))
C
         DO 40 N = 1,NT,2
            BCOEF(IN+N) = -BCOEF(IN+N)
 40         CONTINUE
 50      CONTINUE
C                                    Set up pointer constants:
      IS0   = MORD2 + 1
      NVALS = IS0  + IS0
C
 999  RETURN
      END
      SUBROUTINE IEVERT (DX, IX, SVECT)
C-----------------------------------------------------------------------
C   Compute subscript offset and weights for interpolating at a
C   specified position in a vector.  Interpolation is done using a
C   group of pixels centered on the specified position. The order of
C   interpolation is specified by integer MORD in subroutine SETCOF.
C   We do linear interpolation for MORD=0, cubic for 1, and quintic
C   for 2. If MORD=1 (i.e., cubic interpolation) we will be using
C   four pixels in the interpolation.  From the Everett interpolation
C   package originally coded by Larry Goad at KPNO.
C   Inputs:  DX     R       Position in vector
C   Outputs: IX     I       Offset to start
C            SVECT  R(*)    Weights
C-----------------------------------------------------------------------
      INTEGER   IX
      REAL      SVECT(*), DX
      INTEGER   NR, IR0, IV0, MC, IR
      REAL      U, W, CW, CU, W2, U2
      INCLUDE 'SLICE2.INC'
C-----------------------------------------------------------------------
      IX = DX
      U  = DX - IX
      IX = IX - IS0
      CALL RFILL (NVALS, 0.0, SVECT)
C                                       distance of point from cells
      W  = 1.0 - U
      CW = W
      CU = U
      W2 = W * W
      U2 = U * U
      SVECT(IS0)   = W
      SVECT(IS0+1) = U
C                                       leave if on cell or linear
      IF ((U.EQ.0) .OR. (MORD2.EQ.0)) GO TO 999
      NR  = 1
      IR0 = 0
      IV0 = IS0 - 1
      DO 20 MC = 1,MORD2
C                                       (IR0=MC*MC):
         IR0 = IR0 + NR
C                                       (NR=2*MC+1):
         NR  = NR  + 2
C                                       (IV0=IS0-MC-1):
         IV0 = IV0 - 1
         CU  = CU * (U2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         CW  = CW * (W2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         DO 10 IR = 1,NR
            SVECT(IV0+IR)   = SVECT(IV0+IR)   + CW * BCOEF(IR0+IR)
            SVECT(IV0+IR+1) = SVECT(IV0+IR+1) + CU * BCOEF(IR0+IR)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BINOM (X, M, VAL)
C-----------------------------------------------------------------------
C   BINOM generates binomial coefficients for use in the Everett
C   interpolation routines. It is called only by SETCOF.  From the
C   Everett interpolation package originally coded by Larry Goad, KPNO.
C-----------------------------------------------------------------------
      INTEGER    M
      REAL       X, VAL(*)
      INTEGER    I
      REAL       R, XL
C-----------------------------------------------------------------------
      VAL(1) = 1.
      R = 0.
      XL = X + 1.
C
      DO 20 I = 1,M
         XL = XL - 1.
         R = R + 1.
         VAL(I+1) = VAL(I) * XL / R
 20      CONTINUE
C
      RETURN
      END
      SUBROUTINE SLPUT (DX, DY, ISTUF, SLVAL, MINMAX, SBLK, IERR)
C-----------------------------------------------------------------------
C   This routine will sequentialy write to a slice file created
C   with MAKSLI.  The routine will update counters in ISTUF and keep
C   track of the min and max.
C   Inputs:
C      DX       R        X position.
C      DY       R        Y position.
C      ISTUF    I(6)     (1) is the LUN.
C                        (2) is the FTAB index.
C                        (3) ...
C                        (4) is the last element used in SBLK. 0= none.
C                        (5) is the block in file to write current SBLK.
C                        (6) is the slice version number
C      SLVAL    R        Value of next slice point.
C   In/Out:
C      MINMAX   R(2)     minimum and maximum value in slice.
C      SBLK     R(256)   IO buffer.
C   Output:
C      IERR     I        Error code 0=ok.
C-----------------------------------------------------------------------
      REAL      DX, DY, SLVAL, MINMAX(2), SBLK(256)
      INTEGER   ISTUF(5), IERR
C
      INTEGER   I, ITRIM
      CHARACTER LINE*80
      DOUBLE PRECISION X, Y, Z
      INCLUDE 'SLICE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      ISTUF(4) = ISTUF(4) + 1
      I = ISTUF(4)
      SBLK(I) = SLVAL
C                                       Write to text file
      IF (WFILE) THEN
         CALL XYVAL (DX, DY, X, Y, Z, I)
         IF (AXTYP(LOCNUM).LE.1) Z = ZZ
         WRITE(LINE,1010) DX, DY, SLVAL, X, Y, Z
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:ITRIM(LINE)), IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Write this buffer.
      IF (ISTUF(4).GE.256) THEN
         CALL ZFIO ('WRIT', ISTUF(1), ISTUF(2), ISTUF(5), SBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         ISTUF(5) = ISTUF(5) + 1
         ISTUF(4) = 0
         END IF
C                                       Update the max and min.
      IF (SLVAL.NE.FBLANK) THEN
         MINMAX(1) = MIN (MINMAX(1), SLVAL)
         MINMAX(2) = MAX (MINMAX(2), SLVAL)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (F9.3,F10.3,1PE15.6,3(1PE15.7))
      END
      SUBROUTINE SLCLS (ISTUF, MINMAX, SBLK, IERR)
C-----------------------------------------------------------------------
C   This routine will perform some cleanup functions for a new slice
C   file created with MAKSLI and written into with SLPUT.  This routine
C   will write the last buffer if needed, update the max and min in the
C   slice file header (block two), close the file.
C   Inputs:
C      ISTUF    I(6)     (1) is the LUN.
C                        (2) is the FTAB index.
C                        (3)
C                        (4) is the last element used in SBLK. 0= none.
C                        (5) is the block in file to write current SBLK.
C                        (6) slice version number.
C      MINMAX   R(2)     minimum and maximum value in slice.
C   In/Out:
C      SBLK     R(256)   IO buffer.
C   Output:
C      IERR     I        Error code 0=ok.
C-----------------------------------------------------------------------
      REAL      MINMAX(2), SBLK(256)
      INTEGER   ISTUF(6), IERR
      INCLUDE 'SLICE.INC'
C-----------------------------------------------------------------------
C                                       Save last buffer.
      IF (ISTUF(4).GT.0) THEN
         CALL ZFIO ('WRIT', ISTUF(1), ISTUF(2), ISTUF(5), SBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Update the max and min.
      CALL ZFIO ('READ', ISTUF(1), ISTUF(2), 2, SBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Max-min after 11+NPARM
      SBLK(34) = MINMAX(1)
      SBLK(35) = MINMAX(2)
      CALL ZFIO ('WRIT', ISTUF(1), ISTUF(2), 2, SBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close file.
      CALL ZCLOSE (ISTUF(1), ISTUF(2), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close text file
      IF (WFILE) THEN
         CALL ZTXCLS (LUNPR, PFIND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE FQTERP (IFQN, SLROW, FRQS)
C-----------------------------------------------------------------------
C   Smooths data from irregular grid to regular one
C   Inputs:
C      IFQN    I      Number input FQID points
C      FRQS    D(*)   Frequencies
C   In/out:
C      SLROW   R(*)   In: IFQN values
C                     Out: 4*IFQN-3 values on regular grid
C-----------------------------------------------------------------------
      INTEGER   IFQN
      REAL      SLROW(*)
      DOUBLE PRECISION FRQS(*)
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      SB(MAXIMG), WT(MAXIMG), W
      INTEGER   NG(MAXIMG), NB(MAXIMG), IN, OUT, LNZI
      DOUBLE PRECISION FQFINC, FW, FR, F
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LNZI = 4 * IFQN - 3
      FQFINC = (FRQS(IFQN) - FRQS(1)) / (LNZI - 1)
      CALL RFILL (LNZI, 0.0, SB)
      CALL RFILL (LNZI, 0.0, WT)
      CALL FILL (LNZI, 0, NG)
      CALL FILL (LNZI, 0, NB)
      FW = 2.5D0 * FQFINC
C                                       convolve
      DO 30 IN = 1,IFQN
         FR = FRQS(IN)
         F = FRQS(1) - FQFINC
         DO 20 OUT = 1,LNZI
            F = F + FQFINC
            W = ((FR - F) / FW) ** 2
            IF (W.LT.10.D0) THEN
               IF (SLROW(IN).NE.FBLANK) THEN
                  W = EXP(-W)
                  WT(OUT) = WT(OUT) + W
                  SB(OUT) = SB(OUT) + W * SLROW(IN)
                  NG(OUT) = NG(OUT) + 1
               ELSE
                  NB(OUT) = NB(OUT) + 2
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       average
      DO 40 OUT = 1,LNZI
         IF ((WT(OUT).GT.0.0) .AND. (NB(OUT).LT.NG(OUT))) THEN
            SLROW(OUT) = SB(OUT) / WT(OUT)
         ELSE
            SLROW(OUT) = FBLANK
            END IF
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SLNDOW (NAX, NPTS, KEEP, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   WINDOW interprets user window BLC, TRC into usable values
C   special version allows TRC < BLC
C   Inputs:
C      NAX   I        Number of axes
C      NPTS  I(7)     Number of points / axis
C   In/out:
C      BLC   R(7)     Bottom left corner
C      TRC   R(7)     Top right corner
C   Output:
C      IERR  I        Number of bad axes (0 => ok)
C-----------------------------------------------------------------------
      INTEGER    NAX, NPTS(7), IERR
      REAL       KEEP, BLC(7), TRC(7)
C
      LOGICAL    BEQU
      INTEGER    I, N, J
C-----------------------------------------------------------------------
C                                        BLC = const (all same?)
      BEQU = .TRUE.
      DO 10 I = 1,7
         IF (BLC(I).NE.BLC(1)) BEQU = .FALSE.
 10      CONTINUE
C                                        Set null axes
      IF (NAX.LT.7) THEN
         N = NAX + 1
         DO 15 I = N,7
            BLC(I) = 1.0
            TRC(I) = 1.0
 15         CONTINUE
         END IF
C                                        In range: set,check
      IERR = 0
      DO 30 I = 1,NAX
         IF (NPTS(I).LE.1) THEN
            BLC(I) = 1.0
            TRC(I) = 1.0
            END IF
         IF (TRC(I).LE.0.89) TRC(I) = NPTS(I)
         IF (TRC(I).GT.NPTS(I)) TRC(I) = NPTS(I)
         IF (BLC(I).LT.1.0) BLC(I) = 1.0
         IF (BLC(I).GT.NPTS(I)) BLC(I) = NPTS(I)
 30      CONTINUE
C                                       round off to integer
      IF ((KEEP.GT.0.0) .AND. ((TRC(1)-BLC(1).EQ.0.0) .OR.
     *   (TRC(2)-BLC(2).EQ.0.0))) THEN
         DO 45 I = 1,2
            J = BLC(I) + 0.01
            BLC(I) = J
            J = TRC(I) + 0.89
            TRC(I) = J
 45         CONTINUE
         END IF
      DO 50 I = 3,NAX
         N = BLC(I) + 0.5
         J = TRC(I) + 0.5
         IF (N.LE.J) THEN
            BLC(I) = N
            TRC(I) = J
         ELSE
            BLC(I) = J
            TRC(I) = N
            END IF
 50      CONTINUE
C
 999  RETURN
      END

LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=28)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1        2         3          4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           5           6          7          8          9         10
     *   'TIMERANG', 'DOCALIB', 'GAINUSE', 'FLAGVER', 'STOKES', 'BIF',
C           11       12       13         14          15         16
     *   'BCHAN', 'ECHAN', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
C           17        18       19        20          21       22
     *   'OPTYPE', 'APARM', 'IMSIZE', 'CELLSIZE', 'ROTATE', 'SHIFT',
C           23          24       25       26       27       28
     *   'REWEIGHT', 'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'BADDISK'/
C                    1       2       3       4
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
C          5       6       7       8       9       10
     *   OOARE,  OOALOG, OOAINT, OOAINT, OOACAR, OOAINT,
C          11      12      13      14      15      16
     *   OOAINT, OOAINT, OOACAR, OOACAR, OOAINT, OOAINT,
C          17      18      19      20      21      22
     *   OOACAR, OOARE,  OOAINT, OOARE,  OOARE,  OOARE,
C          23      24      25      26      27      28
     *   OOARE,  OOAINT, OOAINT, OOARE,  OOARE,  OOAINT/
C                   1     2     3     4
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,
C         5     6     7     8     9     10
     *   8,1,  1,1,  1,1,  1,1,  4,1,  1,1,
C          11   12    13    14    15    16
     *   1,1,  1,1, 12,1,  6,1,  1,1,  1,1,
C         17    18    19    20    21    22
     *   4,1, 10,1,  2,1,  2,1,  1,1,  2,1,
C         23    24    25    26    27    28
     *   2,1,  1,1,  1,1, 10,1, 10,1, 10,1/
LOCAL END
      PROGRAM SDGRD
C-----------------------------------------------------------------------
C! Singledish data selection, projection and gridding
C# Task AP Imaging OOP SINGLEDISH
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 2000, 2015
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
      CHARACTER PRGM*6, SDIMAG*32, UVDATA*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SDGRD'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL SDGRIN (PRGM, UVDATA, SDIMAG, IRET)
C                                       grid
      IF (IRET.EQ.0) CALL SDGRIT (UVDATA, SDIMAG, IRET)
C                                       History
      IF (IRET.EQ.0) CALL SDGRHI (UVDATA, SDIMAG)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SDGRIN (PRGN, UVDATA, SDIMAG, IRET)
C-----------------------------------------------------------------------
C   SDGRIN gets input parameters for IMAGR and creates the SDIMAG object.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      SDIMAG   C*32   Name of IMAGE object (contains output objects)
C      UVDATA   C*32   Name of input uv data.
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, SDIMAG*(*), UVDATA*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       UVDATA object
      PARAMETER (NKEY1=16)
C                                       NKEY2 = no. adverb for SD image
      PARAMETER (NKEY2=11)
      INCLUDE 'INPUT.INC'
      INTEGER   DIM(7), TYPE, BCHAN, ECHAN, NAXIS(7), IMSI(2), IWT,
     *   DUMMY, BIF, EIF, IFINDX, FQINDX, IDDEG
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, STOKES*4, UVTYPE*2, CHTYPE*4, TNAME*12,
     *   TCLASS*6, CDUMMY*1
      REAL      APARM(10), RWT(2)
      DOUBLE PRECISION COORD(2)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTNAME',
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ', 'BCHAN',
     *   'ECHAN', 'BIF', 'STOKES',
     *   'DOCALIB', 'GAINUSE', 'FLAGVER',
     *   'TIMERANG'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'OUTNAME',
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ', 'CALEDIT.BCHAN',
     *   'CALEDIT.ECHAN', 'CALEDIT.BIF', 'CALEDIT.STOKES',
     *   'CALEDIT.DOCAL', 'CALEDIT.CLUSE', 'CALEDIT.FGVER',
     *   'CALEDIT.TIMRNG'/
C                                       Adverbs for SDIMAG image object
C                  1          2           3         4          5
      DATA INK2 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'CELLSIZE',
C          6         7         8        9        10       11
     *   'SHIFT',  'ROTATE', 'XTYPE', 'YTYPE', 'XPARM', 'YPARM'/
C                                       Rename
C                   1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CELLSIZE',
C          6        7         8        9        10       11
     *   'SHIFT', 'ROTATE', 'CTYPX', 'CTYPY', 'XPARM', 'YPARM'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default output Name = input
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TNAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TNAME.EQ.' ') THEN
         CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TNAME, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TNAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Default output Class = TSKNAM
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TCLASS.EQ.' ') THEN
         TCLASS = TSKNAM
         CALL OPUT ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Set default stokes.
      CALL OGET ('Input', 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (STOKES .EQ. ' ') STOKES = 'I'
      CALL OPUT ('Input', 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create SDIMAG object
      SDIMAG = 'SDIMAG process object'
      CALL CREATE (SDIMAG, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, SDIMAG, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create other objects
C                                       UVDATA
      UVDATA = 'Input UVdata'
      CALL CREATE (UVDATA, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get IF channel selection
      CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVDATA, 1, 0, 1, 0, '    ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open UVDATA to be sure it's OK.
      CALL OOPEN (UVDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get type of data
      CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((UVTYPE.NE.'SD') .AND. (UVTYPE.NE.'SB')) THEN
         MSGTXT = 'I ONLY WORK ON SD OR SB DATA NOT ''' // UVTYPE //
     *      ''''
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
C                                       Reset Selection: one IF
      EIF = BIF
      CALL SECSAV (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       SDIMAG
C                                       Attach UVDATA to image
      DIM(1) = LEN (UVDATA)
      DIM(2) = 1
      CALL OPUT (SDIMAG, 'UVDATA', OOACAR, DIM, DUMMY, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min. image sizes
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       No images smaller than 32
C                                       and must be even
      IMSI(1) = MAX (IMSI(1), 32)
      IMSI(2) = MAX (IMSI(2), 32)
      IMSI(1) = ((IMSI(1) + 1) / 2) * 2
      IMSI(2) = ((IMSI(2) + 1) / 2) * 2
      CALL OPUT ('Input', 'IMSIZE', OOAINT, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDIMAG, 'IMSIZE', OOAINT, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Coordinates
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      COORD(1) = (APARM(1) + APARM(2)/60. + APARM(3)/3600.) * 15.0
      IDDEG = ABS (APARM(4))
      COORD(2) = (IDDEG + APARM(5)/60. + (APARM(6)/3600.)) *
     *   SIGN (1.0, APARM(4))
      DIM(1) = 2
      CALL OPUT (SDIMAG, 'CCENTER', OOADP, DIM, COORD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OPTYPE', TYPE, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (CHTYPE.EQ.' ') CHTYPE = '-SIN'
      CALL OPUT (SDIMAG, 'CPROJ', OOACAR, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Image type, cutoff
      CALL OGET ('Input', 'REWEIGHT', TYPE, DIM, RWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((RWT(1).GT.0.0001) .AND. (RWT(1).LT.1.50)) THEN
         IWT = 0
      ELSE IF (ABS(RWT(1)-2.0).LE.0.5) THEN
         IWT = 1
      ELSE IF (ABS(RWT(1)-3.0).LE.0.5) THEN
         IWT = 2
      ELSE
         IWT = -1
         IF (RWT(2).EQ.0.0) RWT(2) = -0.01
         END IF
      DIM(1) = 1
C                                        SD type, cutoff
      CALL OPUT (SDIMAG, 'SDTYPE', OOAINT, DIM, IWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDIMAG, 'SDCUTOFF', OOARE, DIM, RWT(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default BCHAN, ECHAN
      CALL OGET ('Input', 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Find size of freq. axis.
      CALL UVDFND (UVDATA, 2, 'FREQ', FQINDX, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (UVDATA, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      BCHAN = MAX (BCHAN, 1)
      BCHAN = MIN (BCHAN, NAXIS(FQINDX))
      IF (ECHAN.LT.BCHAN) ECHAN = NAXIS(FQINDX)
      ECHAN = MIN (ECHAN, NAXIS(FQINDX))
      DIM(1) = 1
      CALL OPUT (SDIMAG, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDIMAG, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save in Inputs for history
      CALL OPUT ('Input', 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       IF numbers
C                                       Default BIF, EIF
      CALL OGET ('Input', 'BIF', TYPE, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVDATA, 2, 'IF  ', IFINDX, IRET)
      IRET = 0
C                                       Find size of IF axis.
      IF (IFINDX.GT.0) THEN
         CALL OGET (UVDATA, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         BIF = MAX (1, BIF)
         BIF = MIN (BIF, NAXIS(IFINDX))
         EIF = BIF
      ELSE
         BIF = 1
         EIF = 1
         END IF
C                                       Save in Inputs for history
      DIM(1) = 1
      CALL OPUT ('Input', 'BIF', OOAINT, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'EIF', OOAINT, DIM, EIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SDGRIT (UVDATA, SDIMAG, IRET)
C-----------------------------------------------------------------------
C   Routine to call OUVIMG to make image
C   Inputs:
C      UVDATA   C*?   Input UV data
C      SDIMAG   C*?   Image process object
C   Output:
C      IRET     I     > 0 => die
C-----------------------------------------------------------------------
      CHARACTER SDIMAG*(*), UVDATA*32
      INTEGER   IRET
C
C-----------------------------------------------------------------------
      CALL OSDIMG (UVDATA, SDIMAG, IRET)
C
 999  RETURN
      END
      SUBROUTINE SDGRHI (UVDATA, SDIMAG)
C-----------------------------------------------------------------------
C   Routine to write history file to output SDIMAG image object.
C   Inputs:
C      UVDATA   C*?   Input UV data
C      SDIMAG   C*?   Image process object
C-----------------------------------------------------------------------
      CHARACTER SDIMAG*(*), UVDATA*32
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NADV0
      PARAMETER (NADV0=23)
      CHARACTER LIST(NADV0)*8, CDUMMY*1, CPROJ*4, LINE*64, OTYPE(4)*16
      INTEGER   IERR, TYPE, DIM(7), I, DUMMY, MSGSAV, IWT, ITRIM
      REAL      PARM(10), MAXCWT, XNLIM, RWT
      DOUBLE PRECISION COORD(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'TIMERANG', 'DOCALIB',
     *   'GAINUSE', 'FLAGVER', 'STOKES', 'BCHAN', 'ECHAN', 'BIF',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'IMSIZE', 'CELLSIZE', 'XTYPE',
     *   'YTYPE', 'XPARM', 'YPARM', 'OPTYPE', 'APARM', 'SHIFT'/
      DATA OTYPE /'Interpolated', 'Convolved', 'Conv weight',
     *   'Sigma**(-2)'/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       Move adverbs to Input
      CALL OGET (SDIMAG, 'XPARM', TYPE, DIM, PARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'XPARM', TYPE, DIM, PARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SDIMAG, 'YPARM', TYPE, DIM, PARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'YPARM', TYPE, DIM, PARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SDIMAG, 'CTYPX', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'XTYPE', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SDIMAG, 'CTYPY', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'YTYPE', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SDIMAG, 'CPROJ', TYPE, DIM, DUMMY, CPROJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'OPTYPE', TYPE, DIM, DUMMY, CPROJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SDIMAG, 'CCENTER', TYPE, DIM, COORD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      COORD(1) = COORD(1) / 15.0D0
      I = COORD(1)
      PARM(1) = I
      COORD(1) = 60.0D0 * (COORD(1) - I)
      I = COORD(1)
      PARM(2) = I
      PARM(3) = 60.0D0 * (COORD(1) - I)
      DUMMY = 1
      IF (COORD(2).LT.0) DUMMY = -1
      COORD(2) = ABS (COORD(2))
      I = COORD(2)
      PARM(4) = I * DUMMY
      COORD(2) = 60.0D0 * (COORD(2) - I)
      I = COORD(2)
      PARM(5) = I
      PARM(6) = 60.0D0 * (COORD(2) - I)
      IF ((DUMMY.LT.0) .AND. (PARM(4).EQ.0.0)) PARM(4) = -1.0E-10
      DIM(1) = 10
      DIM(2) = 1
      CALL OPUT ('Input', 'APARM', OOARE, DIM, PARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy old history
      CALL OHCOPY (UVDATA, SDIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy base adverb values.
      CALL OHLIST ('Input', LIST, NADV0, SDIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       computed results
      CALL OGET (SDIMAG, 'SDTYPE', TYPE, DIM, IWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (SDIMAG, 'SDCUTOFF', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (SDIMAG, 'MAXCWT', TYPE, DIM, MAXCWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SDIMAG, 'XNLIM', TYPE, DIM, XNLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      I = ITRIM (OTYPE(IWT+2))
      WRITE (LINE,1000) OTYPE(IWT+2)(:I)
      CALL OHWRIT (LINE, SDIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((IWT.EQ.0) .OR. (IWT.EQ.1)) THEN
         WRITE (LINE,1001) MAXCWT
         CALL OHWRIT (LINE, SDIMAG, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (RWT.GT.0.0) THEN
         WRITE (LINE,1002) XNLIM
      ELSE
         WRITE (LINE,1003) XNLIM
         END IF
      CALL OHWRIT (LINE, SDIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // SDIMAG
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTTYPE =''',A,'''',3X,' / Form of output')
 1001 FORMAT ('SCALE   =',F9.3,' / scaled down by max conv weight')
 1002 FORMAT ('WEIGHT  =',F9.3,' / Scaled min sum of convolved weights')
 1003 FORMAT ('WEIGHT  =',F9.3,
     *   ' / Scaled min ABS(sum of convolved weights)')
      END

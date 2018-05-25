LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=37)
C
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1        2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'CHANNEL',
C           6      7      8          9          10         11
     *   'BIF', 'EIF', 'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C           12       13         14        15         16          17
     *   'NMAPS', 'ONEFREQ', 'INVERS', 'OUTNAME', 'OUTCLASS', 'OUTSEQ',
C           18         19       20       21      22         23
     *   'OUTDISK', 'BCOMP', 'NCOMP', 'FLUX', 'CMETHOD', 'CMODEL',
C           24        25        26        27       28       29
     *   'FACTOR', 'OPCODE', 'SMODEL', 'BPARM', 'FQTOL', 'IN3NAME',
C           30          31        32         33         34
     *   'IN3CLASS', 'IN3SEQ', 'IN3DISK', 'IN4NAME', 'IN4CLASS',
C           35        36         37
     *   'IN4SEQ', 'IN4DISK', 'BADDISK'/
C                    1       2       3       4        5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,  OOAINT,
C          6       7       8       9       10      11
     *   OOAINT, OOAINT, OOACAR, OOACAR, OOAINT, OOAINT,
C          12      13      14      15      16      17
     *   OOAINT, OOALOG, OOAINT, OOACAR, OOACAR, OOAINT,
C          18      19      20      21     22      23
     *   OOAINT, OOAINT, OOAINT, OOARE, OOACAR, OOACAR,
C          24      25      26     27     28     29
     *   OOARE, OOACAR, OOARE, OOARE, OOARE, OOACAR,
C          30      31      32      33      34
     *   OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C          35      36      37
     *   OOAINT, OOAINT, OOAINT/
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,  1,1,
C         6     7     8     9     10    11
     *   1,1,  1,1, 12,1,  6,1,  1,1,  1,1,
C         12    13    14    15    16    17
     *   1,1,  1,1,  1,1, 12,1,  6,1,  1,1,
C         18        19        20    21    22    23
     *   1,1, MAXAFL,1, MAXAFL,1,  1,1,  4,1,  4,1,
C         24    25    26    27    28    29
     *   1,1,  4,1,  7,1, 10,1,  1,1, 12,1,
C         30    31    32    33    34
     *   6,1,  1,1,  1,1, 12,1,  6,1,
C         35    36    37
     *   1,1,  1,1, 10,1/
LOCAL END
      PROGRAM OOSUB
C-----------------------------------------------------------------------
C! model subration/division with freq-dependent corrections
C# Task AP Imaging OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 2007-2008, 2015-2017
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
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGM*6, OPCODE*4, CLEAN(MAXFLD)*32, UVIN*32, UVOUT*32
      INTEGER   IRET, BUFF1(256), CHAN, NCHAN
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'OOSUB'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL OOSUIN (PRGM, OPCODE, CHAN, NCHAN, CLEAN, UVIN, UVOUT, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL OOSUDO (OPCODE, CHAN, NCHAN, CLEAN, UVIN,
     *   UVOUT, IRET)
C                                       History
      IF (IRET.EQ.0) CALL OOSUHI (UVIN, UVOUT)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE OOSUIN (PRGN, OPCODE, CHAN, NCHAN, CLEAN, UVIN, UVOUT,
     *   IRET)
C-----------------------------------------------------------------------
C  OOSUIN gets input parameters for OOSUB, creates objects and output
C  file.
C   Inputs:
C      PRGN    C*6    Program name
C   Output:
c      OPCODE  C*4    Type of operation
C      CHAN    I      First channel to process
C      NCHAN   I      Number of channels to process
C      CLEAN   C*32   Names of CLEAN objects
C      UVIN    C*32   Name of input uv data
C      UVOUT   C*32   Name of output UV data
C      IRET    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   CHAN, NCHAN, IRET
      CHARACTER PRGN*6, OPCODE*4, CLEAN(*)*(*), UVIN*(*), UVOUT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NKEY1, NKEY2, NKEY3, NKEY6
C                                       NKEY1=no. adverbs to copy to
C                                       CLEAN objects
      PARAMETER (NKEY1=3)
C                                       NKEY2 = no. adverb for UVIN
      PARAMETER (NKEY2=12)
C                                       NKEY3 = no. adverb for UVOUT
      PARAMETER (NKEY3=6)
C                                       NKEY6 = no. adverbs for SPIX
      PARAMETER (NKEY6=4)
c
      INCLUDE 'INPUT.INC'


      INTEGER   DIM(7), TYPE, J, BCHAN, ECHAN, NAXIS(7), I, DUMMY, BIF,
     *   EIF, FRINDX, BCOMP(MAXFLD), NCOMP(MAXFLD), MFIELD, J0, IROUND,
     *   IFINDX, LBIF, LEIF
      LOGICAL   NONEG, WASOME, OLDNAM, COMPRS
      REAL      OOPARM(10), FACTOR, SMODEL(7), FACGRD(2)
      DOUBLE PRECISION PBFREQ(MAXCIF), UVFREQ
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32, INK6(NKEY6)*8,
     *   OUTK6(NKEY6)*32, FTTYPE*4, SPIX*32, STOKES*4, CLAT*6, CMODL*4,
     *   CLASS*6, TINAME*12, TONAME*12, CDUMMY*1, UVTYPE*2, TCLASS*6,
     *   INK6B(NKEY6)*8, SPIXC*32
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for CLEAN image object
      DATA INK1 /'IN2NAME', 'IN2SEQ', 'IN2DISK'/
C                                       Rename
      DATA OUTK1 /'NAME', 'IMSEQ', 'DISK'/
C                                       Adverbs for UVIN object
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'CMETHOD',
     *   'FLUX', 'FQTOL', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'ONEFREQ'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'MODMETH',
     *   'MODFLUX', 'FQTOL', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'MODSFREQ'/
C                                       Adverbs for UVOUT object
      DATA INK3 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'CMETHOD',
     *   'FLUX'/
C                                       Rename
      DATA OUTK3 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'MODMETH',
     *   'MODFLUX'/
C                                       Adverbs for SPIX image object
C                    1          2          3         4
      DATA INK6 /'IN3NAME', 'IN3CLASS', 'IN3SEQ', 'IN3DISK'/
      DATA INK6B /'IN4NAME', 'IN4CLASS', 'IN4SEQ', 'IN4DISK'/
C                                       Rename
C                    1       2          3        4
      DATA OUTK6 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default NFIELD=1
      CALL OGET ('Input', 'NMAPS', TYPE, DIM, MFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (MFIELD.LE.0) THEN
         MFIELD = 1
         CALL OPUT ('Input', 'NMAPS', OOAINT, DIM, MFIELD, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default output Name = input
      CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TINAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TONAME.EQ.' ') THEN
         TONAME = TINAME
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       UVIN
      UVIN = 'Input UVdata'
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get IF channel selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open UVIN to be sure it's OK.
      CALL OOPEN (UVIN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'I DO NOT WORK FOR UV DATA OF TYPE ''' // UVTYPE //
     *      ''''
         IRET = 9
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL OCLOSE (UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reset Selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       UVOUT
      UVOUT = 'UVdata output object'
      CALL CREATE (UVOUT, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Component numbers
      CALL OGET ('Input', 'NCOMP', TYPE, DIM, NCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BCOMP', TYPE, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NONEG = .FALSE.
      WASOME = .FALSE.
      DO 10 I = 1,MFIELD
         IF (I.LE.MAXAFL) THEN
            BCOMP(I) = MAX (1, BCOMP(I))
            IF (NCOMP(I).LT.0) NONEG = .TRUE.
            NCOMP(I) = ABS (NCOMP(I))
            IF (NCOMP(I).GT.0) WASOME = .TRUE.
         ELSE
            BCOMP(I) = 1
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 10      CONTINUE
      DIM(1) = MFIELD
      CALL OPUT (UVIN, 'MODCCBEG', OOAINT, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODCCBEG', OOAINT, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'MODCCEND', OOAINT, DIM, NCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODCCEND', OOAINT, DIM, NCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'INVERS', TYPE, DIM, I, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      I = MAX (1, I)
      CALL FILL (MFIELD, I, BCOMP)
      DIM(1) = MFIELD
      CALL OPUT (UVIN, 'MODCCVER', OOAINT, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODCCVER', OOAINT, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (UVIN, 'MODNONEG', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODNONEG', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NONEG = .FALSE.
      CALL OPUT (UVIN, 'MODDOMSG', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODDOMSG', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'CMODEL', TYPE, DIM, DUMMY, CMODL, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (CMODL.NE.'IMAG') CMODL = 'CC  '
      CALL OPUT (UVIN, 'MODMODEL', OOACAR, DIM, DUMMY, CMODL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODMODEL', OOACAR, DIM, DUMMY, CMODL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Factor
      CALL OGET ('Input', 'OPCODE', TYPE, DIM, DUMMY, OPCODE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'FACTOR', TYPE, DIM, FACTOR, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      FACGRD(1) = FACTOR
      IF (ABS(FACGRD(1)).LT.1.0E-20) FACGRD(1) = 1.0
      IF ((OPCODE.NE.'MODL') .AND. (OPCODE.NE.'DIV ')) OPCODE = 'SUB'
      IF (OPCODE.EQ.'MODL') THEN
         FACGRD(1) = ABS (FACGRD(1))
         FACGRD(2) = 0.0
      ELSE
         FACGRD(2) = 1.0
         END IF
      DIM(2) = 2
      CALL OPUT (UVIN, 'MODFACT', OOARE, DIM, FACGRD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODFACT', OOARE, DIM, FACGRD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       SMODEL
      CALL OGET ('Input', 'SMODEL', TYPE, DIM, SMODEL, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NONEG = ABS (SMODEL(1)).GT.1.E-20
      DIM(1) = 1
      CALL OPUT (UVIN, 'MODDOPT', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODDOPT', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'MODPTFLX', OOARE, DIM, SMODEL(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODPTFLX', OOARE, DIM, SMODEL(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'MODPTXOF', OOARE, DIM, SMODEL(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODPTXOF', OOARE, DIM, SMODEL(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'MODPTYOF', OOARE, DIM, SMODEL(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODPTYOF', OOARE, DIM, SMODEL(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 4
      CALL OPUT (UVIN, 'MODPARMS', OOARE, DIM, SMODEL(4), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'MODPARMS', OOARE, DIM, SMODEL(4), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Channel stuff
      CALL OGET ('Input', 'CHANNEL', TYPE, DIM, CHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BIF', TYPE, DIM, LBIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'EIF', TYPE, DIM, LEIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVIN, 2, 'FREQ', FRINDX, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVIN, 2, 'IF', IFINDX, IRET)
      IF (IRET.GT.1) GO TO 999
      CALL OGET (UVIN, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      ECHAN = NAXIS(FRINDX)
      EIF = 1
      IF (IFINDX.GT.0) EIF = NAXIS(IFINDX)
      LBIF = MAX (1, LBIF)
      IF (LBIF.GT.EIF) LBIF = EIF
      IF (LEIF.GT.EIF) LEIF = EIF
      IF (LEIF.LT.LBIF) LEIF = EIF
C                                       specified single channel
      IF ((CHAN.GE.1) .AND. (CHAN.LE.ECHAN)) THEN
         LEIF = LBIF
         NCHAN = 1
C                                       all spectral channels
      ELSE
         CHAN = 1
         NCHAN = ECHAN
         END IF
C                                       what range of IFs
      IF (IFINDX.GE.0) THEN
         IF (IFINDX.GT.FRINDX) THEN
            CHAN = ECHAN * (LBIF - 1) + CHAN
         ELSE
            IF (NCHAN.GT.1) THEN
               LBIF = 1
               LEIF = EIF
               END IF
            CHAN = EIF * (CHAN-1) + LBIF
            END IF
         NCHAN = NCHAN * (LEIF - LBIF + 1)
         END IF
C                                       compression stuff
      CALL UVDGET (UVIN, 'ISCOMP', TYPE, DIM, COMPRS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'DOUVCOMP', TYPE, DIM, COMPRS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'DOUVCOMP', TYPE, DIM, COMPRS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       clone object
      CALL OCLONE (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       class of model
      CALL OGET ('Input', 'IN2CLASS', TYPE, DIM, DUMMY, CLAT, IRET)
      IF (IRET.NE.0) GO TO 999
      OLDNAM = (CLAT(4:4).LT.'0') .OR. (CLAT(4:4).GT.'9') .OR.
     *   (CLAT(5:5).LT.'0') .OR. (CLAT(5:5).GT.'9') .OR.
     *   (CLAT(6:6).LT.'0') .OR. (CLAT(6:6).GT.'9')
      IF (.NOT.OLDNAM) THEN
         IF ((CLAT(3:3).LT.'0') .OR. (CLAT(3:3).GT.'9')) THEN
            READ (CLAT(4:6),1009) J0
         ELSE
            READ (CLAT(3:6),1008) J0
            END IF
         END IF
C                                       CLEAN
      CLASS = CLAT
      DO 50 I = 1,MFIELD
         WRITE (CLEAN(I),1000) I
         CALL CREATE (CLEAN(I), 'IMAGE', IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, CLEAN(I), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Class
         IF (OLDNAM) THEN
            J = I - 1
            IF (I.NE.1) CALL ZEHEX (J, 2, CLAT(5:6))
         ELSE
            J = I - 1 + J0
            WRITE (TCLASS,1010) J
            IF (J.LE.999) THEN
               CLASS(4:6) = TCLASS(4:6)
            ELSE
               CLASS(3:6) = TCLASS(3:6)
               END IF
            END IF
         DIM(1) = LEN (CLASS)
         DIM(2) = 1
         CALL OPUT (CLEAN(I), 'CLASS', OOACAR, DIM, DUMMY, CLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OOPEN (CLEAN(I), 'READ', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDGET (CLEAN(I), 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (CLEAN(I), IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 2
         CALL OPUT (CLEAN(I), 'IMSIZE', OOAINT, DIM, NAXIS, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 50      CONTINUE
C                                       DFT or FFT
      FTTYPE = 'FFT'
      DIM(1) = LEN (FTTYPE)
      CALL OPUT (CLEAN(1), 'FTTYPE', OOACAR, DIM, DUMMY, FTTYPE,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control information.
      CALL OGET ('Input', 'BPARM', TYPE, DIM, OOPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Primary beam corrections
C                                       Freq array
      CALL UVFRQS (UVIN, UVFREQ, PBFREQ, IRET)
      IF (IRET.NE.0) GO TO 999
      NONEG = OOPARM(1).GT.0.0
      I = 0
      IF (NONEG) I = IROUND (OOPARM(2))
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVIN, 'DOPBFM', OOALOG, DIM, NONEG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'PBFSIZ', OOARE, DIM, OOPARM(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'PBOMITCC', OOAINT, DIM, I, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = (ECHAN - BCHAN + 1) * (EIF - BIF + 1)
      CALL OPUT (UVIN, 'PBFREQ', OOADP, DIM, PBFREQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       spectral index image
      IF (OOPARM(3).GT.0.0) THEN
         CALL OGET ('Input', 'IN3NAME', TYPE, DIM, DUMMY, TINAME, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'IN3CLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         IF ((TINAME.NE.' ') .AND. (TCLASS.NE.' ')) THEN
            SPIX = 'Spectral index image'
            CALL CREATE (SPIX, 'IMAGE', IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
            CALL IN2OBJ ('Input', NKEY6, INK6, OUTK6, SPIX, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 1
            DIM(2) = 1
            OOPARM(3) = OOPARM(3) - 0.5
            CALL OPUT (SPIX, 'SPIXRADIUS', OOARE, DIM, OOPARM(3),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Open SPIX to be sure it's OK.
            CALL OOPEN (SPIX, 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (SPIX, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = LEN (SPIX)
            DIM(2) = 1
            DIM(3) = 1
            CALL OPUT (UVIN, 'SPIXIMAGE', OOACAR, DIM, DUMMY, SPIX,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
C                                       curvature?
            CALL OGET ('Input', 'IN4NAME', TYPE, DIM, DUMMY, TINAME,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OGET ('Input', 'IN4CLASS', TYPE, DIM, DUMMY, TCLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((TINAME.NE.' ') .AND. (TCLASS.NE.' ')) THEN
               SPIXC = 'Spectral index curvature image'
               CALL CREATE (SPIXC, 'IMAGE', IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
               CALL IN2OBJ ('Input', NKEY6, INK6B, OUTK6, SPIXC, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Open SPIXC to be sure it's OK.
               CALL OOPEN (SPIXC, 'READ', IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OCLOSE (SPIXC, IRET)
               IF (IRET.NE.0) GO TO 999
               DIM(1) = LEN (SPIXC)
               DIM(2) = 1
               DIM(3) = 1
               CALL OPUT (UVIN, 'SPIXCURV', OOACAR, DIM, DUMMY, SPIXC,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODEL field number ',I4.4)
 1008 FORMAT (I4)
 1009 FORMAT (I3)
 1010 FORMAT (2X,I4.4)
      END
      SUBROUTINE OOSUDO (OPCODE, CHAN, NCHAN, CLEAN, UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C  OOSUIN gets input parameters for OOSUB, creates objects and output
C  file.
C   Inputs:
C      OPCODE  C*4    opeartion type
C      CHAN    I      First channel to process
C      NCHAN   I      Number of channels to process
C      CLEAN   C*32   Names of CLEAN objects
C      UVIN    C*32   Name of input uv data
C      UVOUT   C*32   Name of output UV data
C   Output:
C      IRET    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   CHAN, NCHAN, IRET
      CHARACTER OPCODE*4, CLEAN(*)*(*), UVIN*(*), UVOUT*(*)
C
      INTEGER   MFIELD, TYPE, DIM(7)
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      CALL OGET ('Input', 'NMAPS', TYPE, DIM, MFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       do it
      IF (OPCODE.NE.'DIV ') THEN
         CALL UVSUBM (UVIN, UVOUT, 0, MFIELD, CLEAN, CHAN, NCHAN, IRET)
      ELSE
         CALL UVDIVM (UVIN, UVOUT, MFIELD, CLEAN, CHAN, NCHAN, IRET)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OOSUHI (UVIN, UVOUT)
C-----------------------------------------------------------------------
C   OOSUHI writes history
C   Input:
C      UVIN    C*32   Name of input uv data
C      UVOUT   C*32   Name of output UV data
C   Output
C      IRET    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
C
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER NOTYPE(3)*2
      INTEGER   IRET, DIM(7), DUMMY
      INCLUDE 'INPUTDATA.INC'
      DATA NOTYPE /'AN', 'FQ', 'SU'/
C-----------------------------------------------------------------------
      CALL OHCOPY (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OHLIST ('Input', AVNAME, NPARMS, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 2
      DIM(2) = 3
      DIM(3) = 1
      DIM(4) = 0
      CALL OPUT (UVIN, 'DROPTABS', OOACAR, DIM, DUMMY, NOTYPE, IRET)
      CALL UVCALT (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'PROBLEM COPYING TABLES TO OUTPUT'
         CALL MSGWRT (7)
         END IF
      GO TO 999
C
 990  MSGTXT = 'PROBLEM WRITING HISTORY TO OUTPUT'
      CALL MSGWRT (6)
C
 999  RETURN
      END

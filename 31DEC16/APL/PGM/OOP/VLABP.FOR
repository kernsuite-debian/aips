LOCAL INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=14)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1         2         3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'ROTATE',
C             6         7          8          9
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C            10         11         12        13
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
C            14
     *   'BADDISK'/
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOARE,
C           6      7       8       9
     *   OOACAR, OOACAR, OOAINT, OOAINT,
C          10      11      12      13
     *   OOACAR, OOACAR, OOAINT, OOAINT,
C          14
     *   OOAINT /
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
C          6    7    8    9
     *   12,1, 6,1, 1,1, 1,1,
C         10   11   12   13
     *   12,1, 6,1, 1,1, 1,1,
C          14
     *   10,1/
LOCAL END
      PROGRAM VLABP
C-----------------------------------------------------------------------
C! VLA antenna beam polarization correction for snapshot images
C# TASK CALIBRATION POLARIZATION  Image VLA OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2015
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
C-----------------------------------------------------------------------
C   VLA snapshot beam polarization correction.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INPUTP*32, INPUTI*32, ANTPOL*32, OUTPUT*32,
     *   SCINPP*32, SCINPI*32, SCOUT*32, SCANT*32, SCRAT*32, CDUM*1
      REAL      PA, PAROT(2)
      INTEGER  IRET, BUFF1(256), TYPE, DIM(7)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'VLABP '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL VBPIN (PRGM, INPUTP, INPUTI, ANTPOL, OUTPUT, SCINPP, SCINPI,
     *   SCOUT, SCANT, SCRAT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate antenna poln
      CALL VBPINT (ANTPOL, SCANT, SCRAT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Rotate beam polarization by
C                                       parallactic angle.
      CALL OGET ('Input', 'ROTATE', TYPE, DIM, PA, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      PAROT(1) = COS (2.0 * PA * DG2RAD)
      PAROT(2) = SIN (2.0 * PA * DG2RAD)
      CALL ARRSMU (SCANT, PAROT, SCANT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Convert input to scratch,
C                                       complex image.
      CALL CX2IMG (INPUTP, SCINPP, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Convert I pol image to complex
      CALL IMGR2C (INPUTI, SCINPI, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Ipol times antenna pattern
      CALL IMGMUL (SCINPI, SCANT, SCINPI, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Correct
      CALL IMGSUB (SCINPP, SCINPI, SCOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write output.
      CALL  IMG2CX (SCOUT, OUTPUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL VBPHIS (INPUTP, OUTPUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE VBPIN (PRGN, INPUTP, INPUTI, ANTPOL, OUTPUT, SCINPP,
     *   SCINPI, SCOUT, SCANT, SCRAT, IRET)
C-----------------------------------------------------------------------
C   VBPIN gets input parameters for VLABP and creates the input, output
C   and scratch objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INPUTP  C*?  Input polarization CX_IMAGE
C      INPUTI  C*?  Input total intensity image
C      ANTPOL  C*?  Antenna polarization CX_IMAGE
C      OUTPUT  C*?  Output polarization CX_IMAGE
C      SCINPP  C*?  Scratch input poln complex IMAGE
C      SCINPI  C*?  Scratch input I complex IMAGE
C      SCOUT   C*?  Scratch output poln complex IMAGE
C      SCANT   C*?  Scratch antenna poln complex IMAGE
C      SCRAT   C*?  Scratch CX_Image object, of size of image to be
C                   interpolated.
C      IRET    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, INPUTP*(*), INPUTI*(*), ANTPOL*(*), OUTPUT*(*),
     *   SCINPP*(*), SCINPI*(*), SCOUT*(*), SCANT*(*), SCRAT*(*)
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       Input images
      PARAMETER (NKEY1=9)
C                                       NKEY2=no. adverbs to copy to
C                                       ANTPOL
      PARAMETER (NKEY2=3)
      INTEGER   IERR, DIM(7), TYPE, MSGSAV, IMSI(7), BLC(7), TRC(7),
     *   NAXIS(7), IDUM, IMSEQ
      REAL      PANGLE, CDELT(7), SCRPIX(7)
      DOUBLE PRECISION CRVAL(7), OBSRA, OBSDEC
      LOGICAL   POLCUB
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, RINPP*32, IINPP*32, ROUT*32, IOUT*32, ANTR*32,
     *   ANTI*32, SCRR*32, SCRI*32, CNAME*8, KEYW*8, DATYPE*8, INC*6,
     *   IN2C*6, OUTC*6, ACLASS*6, CTYPE(7)*8, CDUM*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to input objects
C                    1         2         3         4        5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'ROTATE',
C            6            7         8          9
     *   'OUTNAME',  'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                   1        2        3        5        6
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'PARANGLE',
C            6           7         8         9
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       Adverbs to copy to antenna
C                                       objects
C                     1          2          3
      DATA INK2 /'IN2NAME', 'IN2SEQ', 'IN2DISK'/
C                    1       2       3
      DATA OUTK2 /'NAME', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default classes
      CALL OGET ('Input', 'INCLASS', TYPE, DIM, IDUM, INC, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (INC.EQ.' ') INC = 'ICL001'
      CALL OPUT ('Input', 'INCLASS', TYPE, DIM, IDUM, INC, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'IN2CLASS', TYPE, DIM, IDUM, IN2C, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (IN2C.EQ.' ') IN2C = 'QFRACT'
      CALL OPUT ('Input', 'IN2CLASS', TYPE, DIM, IDUM, IN2C, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, IDUM, OUTC, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OUTC.EQ.' ') OUTC = 'QCORR'
      CALL OPUT ('Input', 'OUTCLASS', TYPE, DIM, IDUM, OUTC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input objects:
C                                       polarization
      RINPP = 'Input poln. Real'
      CALL CREATE (RINPP, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
      IINPP = 'Input poln. Imaginary'
      CALL CREATE (IINPP, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
      INPUTI = 'Input total intensity'
      CALL CREATE (INPUTI, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, RINPP, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, IINPP, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, INPUTI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open INPUTI and see if its a
C                                       polarization cube
      CALL OOPEN (INPUTI, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get image info
      CALL ARDGET (INPUTI, 'NAXIS', TYPE, DIM, NAXIS, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDGET (INPUTI, 'CRVAL', TYPE, DIM, CRVAL, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNGET (INPUTI, 'OBSRA', TYPE, DIM, OBSRA, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNGET (INPUTI, 'OBSDEC', TYPE, DIM, OBSDEC, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDGET (INPUTI, 'CTYPE', TYPE, DIM, IDUM, CTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDGET (INPUTI, 'CDELT', TYPE, DIM, CDELT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (INPUTI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Is this a polarization cube?
      POLCUB = (CTYPE(3).EQ.'STOKES') .AND. (NAXIS(3).EQ.3) .AND.
     *   ((CRVAL(3)-1.0D0).LT.0.1D0) .AND. ((CDELT(3)-1.0).LT.0.1)
      IF (POLCUB) THEN
C                                       Create output
         ROUT = 'Output real'
         CALL OCLONE (INPUTI, ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
         IOUT = 'Output Imaginary'
         CALL OCOPY (ROUT, IOUT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Polarization cube
C                                       Set BLC, TRC
         CALL FILL (7, 0, BLC)
         CALL FILL (7, 0, TRC)
         BLC(3) = 1
         TRC(3) = 1
         DIM(1) = 7
         DIM(2) = 1
         CALL ARDPUT (INPUTI, 'BLC', OOAINT, DIM, BLC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (INPUTI, 'TRC', OOAINT, DIM, TRC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (ROUT, 'BLC', OOAINT, DIM, BLC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (ROUT, 'TRC', OOAINT, DIM, TRC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy total intensity
         CALL IMCOPY (INPUTI, ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy beam
         CALL OOPEN (ROUT, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL BEMCOP (INPUTI, ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Q
         BLC(3) = 2
         TRC(3) = 2
         CALL ARDPUT (RINPP, 'BLC', OOAINT, DIM, BLC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (RINPP, 'TRC', OOAINT, DIM, TRC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (ROUT, 'BLC', OOAINT, DIM, BLC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (ROUT, 'TRC', OOAINT, DIM, TRC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       U
         BLC(3) = 3
         TRC(3) = 3
         CALL ARDPUT (IINPP, 'BLC', OOAINT, DIM, BLC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (IINPP, 'TRC', OOAINT, DIM, TRC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (IOUT, 'BLC', OOAINT, DIM, BLC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARDPUT (IOUT, 'TRC', OOAINT, DIM, TRC, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy output Sequence number to
C                                       imaginary.
         CALL FNAGET (ROUT, 'IMSEQ', TYPE, DIM, IMSEQ, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL FNAPUT (IOUT, 'IMSEQ', TYPE, DIM, IMSEQ, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
C                                       Separate images
C                                       Add AIPS file classes.
         DIM(1) = LEN (ACLASS)
         DIM(2) = 1
         DIM(3) = 0
         ACLASS = 'Q' // INC(2:)
         CALL OPUT (RINPP, 'FILE_NAME.CLASS', 3, DIM, IDUM, ACLASS,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         ACLASS = 'U' // INC(2:)
         CALL OPUT (IINPP, 'FILE_NAME.CLASS', 3, DIM, IDUM, ACLASS,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         ACLASS = 'I' // INC(2:)
         CALL OPUT (INPUTI, 'FILE_NAME.CLASS', 3, DIM, IDUM, ACLASS,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         ACLASS = 'Q' // OUTC(2:)
         CALL OPUT (RINPP, 'OUTCLASS', 3, DIM, IDUM, ACLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         ACLASS = 'U' // OUTC(2:)
         CALL OPUT (IINPP, 'OUTCLASS', 3, DIM, IDUM, ACLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Open inputs to check
      CALL OOPEN (IINPP, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (IINPP, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OOPEN (RINPP, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (RINPP, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OOPEN (INPUTI, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (INPUTI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Antenna poln.
      ANTR = 'Antenna polarization Real'
      CALL CREATE (ANTR, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
      ANTI = 'Antenna polarization Imaginary'
      CALL CREATE (ANTI, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, ANTR, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, ANTI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Add AIPS file classes.
      DIM(1) = LEN (ACLASS)
      DIM(2) = 1
      DIM(3) = 0
      ACLASS = 'Q' // IN2C(2:)
      CALL OPUT (ANTR, 'FILE_NAME.CLASS', 3, DIM, IDUM, ACLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      ACLASS = 'U' // IN2C(2:)
      CALL OPUT (ANTI, 'FILE_NAME.CLASS', 3, DIM, IDUM, ACLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open inputs to check
      CALL OOPEN (ANTR, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (ANTR, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OOPEN (ANTI, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (ANTI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object - attached
C                                       output file naming to INPUTP
C                                       Only for separate planes, cubes
C                                       were handled earlier
      IF (.NOT.POLCUB) THEN
         ROUT = 'Output real'
         CALL OCLONE (RINPP, ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy beam
         CALL OOPEN (ROUT, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL BEMCOP (RINPP, ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (ROUT, IRET)
         IF (IRET.NE.0) GO TO 999
         IOUT = 'Output Imaginary'
         CALL OCLONE (IINPP, IOUT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy output Sequence number to
C                                       imaginary.
         CALL FNAGET (ROUT, 'IMSEQ', TYPE, DIM, IMSEQ, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL FNAPUT (IOUT, 'IMSEQ', TYPE, DIM, IMSEQ, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy beam
         CALL OOPEN (IOUT, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL BEMCOP (IINPP, IOUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (IOUT, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Create complex image objects
      INPUTP = 'Input poln. image'
      CALL CIMCRE (INPUTP, RINPP, IINPP, IRET)
      IF (IRET.NE.0) GO TO 999
      ANTPOL = 'Antenna polarization'
      CALL CIMCRE (ANTPOL, ANTR, ANTI, IRET)
      IF (IRET.NE.0) GO TO 999
      OUTPUT = 'Output CX_IMAGE'
      CALL CIMCRE (OUTPUT, ROUT, IOUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Declare 'PARANGLE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'PARANGLE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Look for 'PARANGLE' on RINPP
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (RINPP, 'PARANGLE', TYPE, DIM, PANGLE, CDUM, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         CALL OGET ('Input', 'ROTATE', TYPE, DIM, PANGLE, CDUM, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Flip sign or PANGLE for
C                                       correction
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT ('Input', 'ROTATE', OOARE, DIM, PANGLE, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Scratch images
C                                       How big (complex)?
      CALL OGET (INPUTI, 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, IMSI,
     *   CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (IRET.NE.0) GO TO 999
C                                       Only 2-D
      CALL FILL (5, 0, IMSI(3))
C                                       Scratch CX ojbect
C                                       Real part
      SCRR = 'Scratch CX real'
      CALL  IMGSCR (SCRR, IMSI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Establish geometry
      CALL OOPEN (SCRR, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 7
      DIM(2) = 1
      CALL IMDPUT (SCRR, 'CRVAL', OOADP, DIM, CRVAL, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDPUT (SCRR, 'CDELT', OOARE, DIM, CDELT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL FILL (7, 1.0, SCRPIX)
      IF ((ABS (OBSRA).GT.1.0E-10) .AND. (ABS (OBSDEC).GT.1.0E-10)) THEN
C                                       This presumes first two axes are
C                                       RA, Dec.
         CALL PSNPIX (INPUTI, OBSRA, OBSDEC, SCRPIX(1), SCRPIX(2), IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
C                                       Don't know original pointing
C                                       center, assumed at center of
C                                       image.
         SCRPIX(1) = (NAXIS(1) / 2)
         SCRPIX(2) = (1 + NAXIS(2)) / 2
         END IF
      CALL IMDPUT (SCRR, 'CRPIX', OOARE, DIM, SCRPIX, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (SCRR, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Imaginary
      SCRI = 'Scratch CX Imaginary'
      CALL  IMGSCR (SCRI, IMSI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Establish geometry
      CALL OOPEN (SCRI, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDPUT (SCRI, 'CRVAL', OOADP, DIM, CRVAL, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDPUT (SCRI, 'CDELT', OOARE, DIM, CDELT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMDPUT (SCRI, 'CRPIX', OOARE, DIM, SCRPIX, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (SCRI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create complex image objects
      SCRAT = 'Scratch CX object'
      CALL CIMCRE (SCRAT, SCRR, SCRI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Scratch complex image objects
      SCINPI = 'complex total intensity'
      SCINPP = 'complex polarized intensity'
      SCANT = 'complex antenna poln'
      SCOUT = 'complex output polarization'
      IMSI(1) = IMSI(1) * 2
      CALL  IMGSCR (SCOUT, IMSI, IRET)
      IF (IRET.NE.0) GO TO 999
      DATYPE = 'COMPLEX'
      DIM(1) = LEN (DATYPE)
      DIM(2) = 1
      CALL ARDPUT (SCOUT, 'DATATYPE', OOACAR, DIM, IDUM, DATYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE VBPINT (ANTPOL, SCANT, SCRAT, IERR)
C-----------------------------------------------------------------------
C   Interpolate and rotate beam polarization and return as a image of
C   type complex.
C   Inputs:
C      ANTPOL  C*?  Antenna polarization CX_IMAGE, deleted on output.
C      SCRAT   C*?  Scratch CX_Image object, of size of image to be
C                   interpolated.  Zapped on exit.
C   Output:
C      SCANT   C*?  Scratch antenna poln complex IMAGE
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER ANTPOL*(*), SCANT*(*), SCRAT*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), IDUM
      REAL      PANGLE, SHIFT(2), EL, ZA
      CHARACTER SCRR*32, SCRI*32, ANTR*32, ANTI*32, CDUM*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Get names of component parts.
      CALL OGET (ANTPOL, 'REALPART', TYPE, DIM, IDUM, ANTR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (ANTPOL, 'IMAGPART', TYPE, DIM, IDUM, ANTI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        Get names of component parts.
      CALL OGET (SCRAT, 'REALPART', TYPE, DIM, IDUM, SCRR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SCRAT, 'IMAGPART', TYPE, DIM, IDUM, SCRI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Parallactic angle
      CALL OGET ('Input', 'ROTATE', TYPE, DIM, PANGLE, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Zenith angle
C      CALL OGET ('Input', 'ZENANGLE', TYPE, DIM, ZA, CDUM, IERR)
C      IF (IERR.NE.0) GO TO 990
C                                       interpolation parameters
      SHIFT(1) = 0.0
      SHIFT(2) = 0.0
C                                       EL not really used.
      ZA = 45.0
      EL = ZA - 90.0
C                                       Interpolate
      CALL IMGIAE (ANTR, PANGLE, EL, 3, SCRR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGIAE (ANTI, PANGLE, EL, 3, SCRI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert to image of type complex
      CALL CX2IMG (SCRAT, SCANT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Zap Scratch image
      CALL CIMZAP (SCRAT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete ANTPOL object
      CALL CIMDES (ANTPOL, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR INTERPOLATING ' // ANTPOL
      CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE VBPHIS (INPUT, OUTPUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output image object.
C   History is written to both Q and U components of OUTPUT.
C   Inputs:
C      INPUT    C*?  Complex input image
C      OUTPUT   C*?  Complex output image.
C-----------------------------------------------------------------------
      CHARACTER INPUT*(*), OUTPUT*(*)
C
      INTEGER   NADV, IDUM
      PARAMETER (NADV=5)
      CHARACTER RINPUT*32, ROUT*32, IOUT*32, LIST(NADV)*8
      INTEGER   IERR, TYPE, DIM(7)
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INSEQ', 'IN2NAME', 'IN2SEQ', 'ROTATE'/
C-----------------------------------------------------------------------
C                                        Get names of component parts.
      CALL OGET (INPUT, 'REALPART', TYPE, DIM, IDUM, RINPUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTPUT, 'REALPART', TYPE, DIM, IDUM, ROUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTPUT, 'IMAGPART', TYPE, DIM, IDUM, IOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        Copy old history to Q (real)
C                                        OUTPUT
      CALL OHCOPY (RINPUT, ROUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, ROUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy to imaginary (U) part
      CALL OHCOPY (ROUT, IOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUTPUT
      CALL MSGWRT (6)
 999  RETURN
      END

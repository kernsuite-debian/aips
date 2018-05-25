LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=18)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'BLC', 'TRC', 'FACTOR', 'NCOUNT', 'DPARM', 'DOALIGN'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOARE, OOAINT, OOARE, OOALOG/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   7,1, 7,1, 1,1, 1,1, 10,1, 1,1/
LOCAL END
      PROGRAM BSCOR
C-----------------------------------------------------------------------
C! Correctes 2 beam-switched images into one output image
C# Map singledish OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1997, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
      CHARACTER PRGM*6, PLUS*32, MINUS*32, OUT*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BSCOR '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL BSCOIN (PRGM, PLUS, MINUS, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL IMGBSC (PLUS, MINUS, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL BSCOHI (PLUS, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BSCOIN (PRGN, PLUS, MINUS, OUT, IRET)
C-----------------------------------------------------------------------
C   BSCOIN gets input parameters for BSCOR and creates the output.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      PLUS    C*?   plus image input object
C      MINUS   C*?   minus image in object
C      OUT     C*?   Output object
C      IRET    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, PLUS*(*), MINUS*(*), OUT*(*)
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=12)
C                                       NKEY2 = no. adverb for in2name
      PARAMETER (NKEY2=8)
      INTEGER   DIM(7), TYPE, BLC(7), TRC(7), NAXIS(7), IROUND, ORDER,
     *   WIN(4), NX, ODISK, OCNO, BUFFER(512)
      REAL      DPARM(10), THROWP, THROWM
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CNAME*8, KEYW*8, CDUMMY*1
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
     *   'DOALIGN', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'FACTOR'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC',
     *   'DOCHECK', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'FACTOR'/
C                                       Adverbs for in2name
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'BLC',
     *   'TRC', 'DOALIGN', 'FACTOR'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC',
     *   'DOCHECK', 'FACTOR'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Declare 'BSTHROW' a header
C                                       keyword for the image class.
C                                       Also TOTHROW as header word
      CNAME = 'IMAGE'
      KEYW = 'BSTHROW'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
      KEYW = 'TOTHROW'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create PLUS
      PLUS = 'Input plus throw image object'
      CALL CREATE (PLUS, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, PLUS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
      CALL OOPEN (PLUS, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (PLUS, 'BSTHROW', TYPE, DIM, THROWP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (PLUS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create MINUS
      MINUS = 'Input minus throw image object'
      CALL CREATE (MINUS, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, MINUS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
      CALL OOPEN (MINUS, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (MINUS, 'BSTHROW', TYPE, DIM, THROWM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (MINUS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       check coordinates
      CALL IMGCHK (PLUS, MINUS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       baseline order
      CALL OGET ('Input', 'NCOUNT', TYPE, DIM, ORDER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      ORDER = MAX (0, MIN (1, ORDER))
      CALL OPUT ('Input', 'NCOUNT', TYPE, DIM, ORDER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (PLUS, 'BLORDER', TYPE, DIM, ORDER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (MINUS, 'BLORDER', TYPE, DIM, ORDER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Input subimage dimension
      CALL ARRWIN (PLUS, BLC, TRC, NAXIS, IRET)
      IF (IRET.NE.0) GO TO 999
      NX = TRC(1) - BLC(1) + 1
C                                       Control parameters
      CALL OGET ('Input', 'DPARM', TYPE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (DPARM(1).LE.0.0) DPARM(1) = 1.0
      IF (DPARM(2).GT.0.0) THEN
         DPARM(2) = DPARM(1)
         DPARM(1) = 1.0
      ELSE
         DPARM(2) = 1.0
         END IF
      WIN(1) = IROUND (DPARM(3))
      WIN(2) = IROUND (DPARM(4))
      WIN(3) = IROUND (DPARM(5))
      WIN(4) = IROUND (DPARM(6))
      WIN(1) = MAX (1, WIN(1))
      IF (WIN(2).LT.WIN(1)) WIN(2) = NX
      IF (WIN(4).LE.0) WIN(4) = NX
      IF (WIN(3).LE.0) WIN(3) = NX * 10
      DPARM(3) = WIN(1)
      DPARM(4) = WIN(2)
      DPARM(5) = WIN(3)
      DPARM(6) = WIN(4)
C                                       Save DPARM for history
      DIM(1) = 10
      CALL OPUT ('Input', 'DPARM', OOARE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (PLUS, 'REWEIGHT', OOARE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (MINUS, 'REWEIGHT', OOARE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 4
      CALL OPUT (PLUS, 'BLWINDOW', OOAINT, DIM, WIN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (MINUS, 'BLWINDOW', OOAINT, DIM, WIN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone output from PLUS
      OUT = 'Output corrected image'
C                                       Copy array descriptors
      CALL IMGCLN (PLUS, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create
      CALL OOPEN (OUT, 'DEST', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy beam parameters
      CALL BEMCOP (PLUS, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Total throw
      DIM(1) = 1
      THROWP = THROWP - THROWM
      CALL OPUT (OUT, 'TOTHROW', OOARE, DIM, THROWP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (OUT, IRET)
C                                       Clean up keywords
      CALL OBDSKC (OUT, ODISK, OCNO, IRET)
      IF (IRET.NE.0) GO TO 999
      KEYW = 'BSTHROW'
      CALL REMKEY (ODISK, OCNO, KEYW, 1, BUFFER, IRET)
      IF (IRET.GT.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE BSCOHI (PLUS, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      PLUS    C*?  Input object
C      OUT     C*?  Output object
C-----------------------------------------------------------------------
      CHARACTER PLUS*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=13)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'BLC', 'TRC',
     *   'FACTOR', 'NCOUNT', 'DPARM'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (PLUS, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL MSGWRT (6)
 999  RETURN
      END

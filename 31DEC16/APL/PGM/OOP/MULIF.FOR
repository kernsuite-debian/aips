LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=11)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'NUMBIF', 'FQCENTER', 'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOARE, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   1,1, 1,1, 10,1/
LOCAL END
      PROGRAM MULIF
C-----------------------------------------------------------------------
C! Change number of IFs in output
C# Task UV OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012, 2014, 2016
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
      CHARACTER PRGM*6, INPUT*32, UVIN*32, UVOUT*32
      INTEGER   IRET, BUFF1(256)
      DOUBLE PRECISION UVSCAL
      REAL      DIFPIX
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA PRGM /'MULIF'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL MULFIN (PRGM, INPUT, UVIN, UVOUT, UVSCAL, DIFPIX, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Copy
      CALL MULFCP (UVIN, UVOUT, UVSCAL, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Patch FQ table
      CALL MULFQ (UVOUT, DIFPIX, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       History
      CALL MULFHI (INPUT, UVIN, UVOUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE MULFIN (PRGN, INPUT, UVIN, UVOUT, UVSCAL, DIFPIX, IERR)
C-----------------------------------------------------------------------
C   MULFIN gets input parameters for MULIF and creates the input and
C   output uvdata objects.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INPUT   C*?  Task inputs object
C      UVIN    C*?  Input uv data object.
C      UVOUT   C*?  Output uv data
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, INPUT*(*), UVIN*(*), UVOUT*(*)
      DOUBLE PRECISION UVSCAL
      REAL      DIFPIX
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY1=4)
C                                       No. abverbs for UVOUT
      PARAMETER (NKEY2=4)
      INTEGER   DIM(7), TYPE, NUMIF, INDXIF, INDXF,NDIM, NAXIS(7),
     *   NASAVE, LOOP, INDX, SEQO, SEQI, DUMMY
      REAL      CDELT(7), CRPIX(7), CROTA(7), XCENT
      DOUBLE PRECISION CRVAL(7)
      LOGICAL   ADDIF, OLDFIL
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CTYPE(7)*8, NAMEI*12, NAMEO*12, CLASSI*6,
     *   CLASSO*6, CDUMMY*1
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVIN
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK'/
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C                                       Adverbs for UVOUT
      DATA INK2 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup
      INPUT = 'Task Input'
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, INPUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FQCENTER
      CALL OGET (INPUT, 'FQCENTER', TYPE, DIM, XCENT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET (INPUT, 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create UVIN
      UVIN = 'Input UVdata'
      CALL CREATE (UVIN, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY1, INK1, OUTK1, UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVIN to be sure it's OK.
      CALL OOPEN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get name info
      CALL OGET (UVIN, 'NAME', TYPE, DIM, DUMMY, NAMEI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'CLASS', TYPE, DIM, DUMMY, CLASSI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'IMSEQ', TYPE, DIM, SEQI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy autocorrelations
      CALL OPUT (UVIN, 'DOACOR', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Number of IFs
      CALL OGET (INPUT, 'NUMBIF', TYPE, DIM, NUMIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NUMIF.LE.0) NUMIF = 1
C                                       Save for history
      CALL OPUT (INPUT, 'NUMBIF', OOAINT, DIM, NUMIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create output
      UVOUT = 'Output UV data '
      CALL CREATE  (UVOUT, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy descriptors from input
      CALL UVDCOP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY2, INK2, OUTK2, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get Name, Class
      CALL OGET (UVOUT, 'NAME', TYPE, DIM, DUMMY, NAMEO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVOUT, 'CLASS', TYPE, DIM, DUMMY, CLASSO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVOUT, 'IMSEQ', TYPE, DIM, SEQO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy autocorrelations
      CALL OPUT (UVOUT, 'DOACOR', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Defaults
      CALL MAKOUT (NAMEI, CLASSI, SEQI, '      ', NAMEO, CLASSO, SEQO)
C                                       Set Name, Class
      DIM(1) = LEN (NAMEO)
      CALL OPUT (UVOUT, 'NAME', OOACAR, DIM, DUMMY, NAMEO, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = LEN (CLASSO)
      CALL OPUT (UVOUT, 'CLASS', OOACAR, DIM, DUMMY, CLASSO, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (UVOUT, 'IMSEQ', OOAINT, DIM, SEQO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check for frequency axis
      CALL UVDFND (UVOUT, 2, 'FREQ', INDXF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING FREQUENCY AXIS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Check for IF axis
      CALL UVDFND (UVOUT, 2, 'IF  ', INDXIF, IERR)
C                                       Need to add IF axis?
      ADDIF = (IERR.NE.0) .OR. (INDXIF.LE.0)
      IERR = 0
C                                       Get array info
      CALL UVDGET (UVOUT, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVOUT, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      NASAVE = DIM(1)
      CALL UVDGET (UVOUT, 'CTYPE', TYPE, DIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVOUT, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVOUT, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVOUT, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVOUT, 'CROTA', TYPE, DIM, CROTA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Update/add IF axis
      IF (ADDIF) THEN
C                                       Add axis
         NDIM = NDIM + 1
         IF (NDIM.GT.7) THEN
            IERR = 9
            MSGTXT = 'TOO MANY REGULAR AXES'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Add after FREQ, shift array
         INDXIF = INDXF + 1
         INDXF = INDXF + 2
         DO 200 LOOP = INDXF,NDIM
            INDX = NDIM - (LOOP - INDXF)
            NAXIS(INDX) = NAXIS(INDX-1)
            CTYPE(INDX) = CTYPE(INDX-1)
            CRVAL(INDX) = CRVAL(INDX-1)
            CDELT(INDX) = CDELT(INDX-1)
            CRPIX(INDX) = CRPIX(INDX-1)
            CROTA(INDX) = CROTA(INDX-1)
 200        CONTINUE
         NAXIS(INDXIF) = NUMIF
         CTYPE(INDXIF) = 'IF      '
         CRVAL(INDXIF) = 1.0D0
         CDELT(INDXIF) = 1.0
         CRPIX(INDXIF) = 1.0
         CROTA(INDXIF) = 0.0
      ELSE
C                                       Change number
         NAXIS(INDXIF) = NUMIF
         END IF
C                                       frequency reference
      IF (XCENT.GT.0.0) THEN
         INDX = NAXIS(INDXF) / 2 + 1
         DIFPIX = INDX - CRPIX(INDXF)
         UVSCAL = CRVAL(INDXF)
         CRVAL(INDXF) = CRVAL(INDXF) + DIFPIX * CDELT(INDXF)
         CRPIX(INDXF) = INDX
         UVSCAL = CRVAL(INDXF) / UVSCAL
      ELSE
         UVSCAL = 1.0D0
         DIFPIX = 0.0
         END IF
C                                       Save array info
      DIM(1) = 1
      CALL UVDPUT (UVOUT, 'NDIM', OOAINT, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = LEN (CTYPE(1))
      DIM(2) = NASAVE
      CALL UVDPUT (UVOUT, 'CTYPE', OOACAR, DIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = NASAVE
      DIM(2) = 1
      CALL UVDPUT (UVOUT, 'NAXIS', OOAINT, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (UVOUT, 'CRVAL', OOADP, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (UVOUT, 'CDELT', OOARE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (UVOUT, 'CRPIX', OOARE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (UVOUT, 'CROTA', OOARE, DIM, CROTA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open and close to force create
      CALL OOPEN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make sure the file is newly
C                                       created.
      CALL OUVGET (UVOUT, 'OLDFILE', TYPE, DIM, OLDFIL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (OLDFIL) THEN
         IERR = 2
         MSGTXT = 'OUTPUT FILE ALREADY EXISTS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL OCLOSE (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE MULFCP (UVIN, UVOUT, UVSCAL, IERR)
C-----------------------------------------------------------------------
C   Copies one uv data object to another
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object.
C   Output:
C      IERR    I    Error code: 0 => ok
C------------------------------------------------------------x-----------
      CHARACTER UVIN*(*), UVOUT*(*)
      DOUBLE PRECISION UVSCAL
      INTEGER   IERR
C
      INTEGER   COUNT, TYPE, DIM(7), DUMMY, INDXU, INDXV, INDXW, IDISK,
     *   ICNO, ODISK, OCNO, CATSAV(256), RNXRET
      CHARACTER SORD*2
      INCLUDE 'INCS:PUVD.INC'
      REAL     RP(50), VS(3,MAXCIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DRNX.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL UVDFND (UVIN, 1, 'UU', INDXU, IERR)
      CALL UVDFND (UVIN, 1, 'VV', INDXV, IERR)
      CALL UVDFND (UVIN, 1, 'WW', INDXW, IERR)
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find disk and slot
      CALL OBDSKC (UVIN, IDISK, ICNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OBHGET (UVIN, CATSAV, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find disk and slot
      CALL OBDSKC (UVOUT, ODISK, OCNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      CALL OBHGET (UVOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Init vis array
      CALL RFILL (3*MAXCIF, 0.0, VS)
C                                       initial index table
      CALL RNXGET (IDISK, ICNO, CATSAV)
      CALL RNXINI (ODISK, OCNO, CATBLK, RNXRET)
C                                       Initialize visibility count
      COUNT = 0
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       scale u,v,w
         RP(INDXU) = RP(INDXU) * UVSCAL
         RP(INDXV) = RP(INDXV) * UVSCAL
         RP(INDXW) = RP(INDXW) * UVSCAL
C                                       update NX table
         CALL RNXUPD (RP, RNXRET)
C                                       write
         COUNT = COUNT + 1
         CALL UVWRIT (UVOUT, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 990
         GO TO 100
 200     IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'MULFCP: NO DATA SELECTED'
         GO TO 985
         END IF
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       index table close
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         CALL MSGWRT (7)
         END IF
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'MULFCP: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE MULFQ (UVOUT, DIFPIX, IERR)
C-----------------------------------------------------------------------
C   Patch up FQ table
C   Inputs:
C      UVOUT   C*?  Output file.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVOUT*(*)
      REAL      DIFPIX
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER FQTAB*32, CDUMMY*1
      INTEGER  NAXIS(7), INDXIF, NUMIF, IIF, NIF, ISBAND(MAXIF), FREQID,
     *   DIM(3), TYPE, NROW, INDXF
      REAL      FINC(MAXIF), HDRINC(7)
      CHARACTER BNDCOD(MAXIF)*8
      DOUBLE PRECISION FOFF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL UVDFND (UVOUT, 2, 'IF  ', INDXIF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PROBLEM WITH OUTPUT IF AXIS'
         GO TO 985
         END IF
      CALL UVDFND (UVOUT, 2, 'FREQ', INDXF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PROBLEM WITH OUTPUT FREQ AXIS'
         GO TO 985
         END IF
      CALL UVDGET (UVOUT, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVOUT, 'NAXIS', TYPE, DIM, HDRINC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       New number of IFs
      NUMIF = NAXIS(INDXIF)
C                                       Make table object from UVDATA
      FQTAB = 'Temporary FQ table for MULFQ'
      CALL UV2TAB (UVOUT, FQTAB, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get existing FQ IF info
      FREQID = 0
      CALL OCHNDA (FQTAB, 'READ', NIF, FOFF, ISBAND, FINC, BNDCOD,
     *   FREQID, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       correct for frequency offset
      DO 20 IIF = 1,NIF
         FOFF(IIF) = FOFF(IIF) + DIFPIX * (FINC(IIF) - HDRINC(INDXF))
 20      CONTINUE
C                                       Correct table for number IFs
      IF (NUMIF.GT.NIF) THEN
C                                       "New" IFs, copy from IF 1
         DO 100 IIF = NIF+1,NUMIF
            ISBAND(IIF) = ISBAND(1)
            FINC(IIF) = FINC(1)
            FOFF(IIF) = FOFF(1)
            BNDCOD(IIF) = BNDCOD(1)
 100        CONTINUE
         NIF = NUMIF
      ELSE
C                                       Drop old IFs
         NIF = NUMIF
         END IF
C                                       If multiple FQ ids change number
C                                       of IFs
      CALL OOPEN (FQTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (FQTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OCLOSE (FQTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (NROW.GT.1) THEN
C                                       Multiple FQ ids.
C                                       Change keyword 'NO_IF'
         CALL OOPEN (FQTAB, 'WRITE', IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 1
         DIM(2) = 1
         CALL TABPUT (FQTAB, 'KEY.NO_IF', OOAINT, DIM, NIF, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OCLOSE (FQTAB, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Single FQ id.
C                                       Destroy old FQ table
         CALL ZAP (FQTAB, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Recreate
         CALL UV2TAB (UVOUT, FQTAB, 'FQ', 1, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Rewrite FQ table
      CALL OCHNDA (FQTAB, 'WRIT', NIF, FOFF, ISBAND, FINC, BNDCOD,
     *   FREQID, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary object
      CALL OBFREE (FQTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'ERROR PATCHING FQ TABLE FOR ' // UVOUT
      CALL MSGWRT (7)
 999  RETURN
      END
      SUBROUTINE MULFHI (INPUT, UVIN, UVOUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output UV data object
C   Inputs:
C      INPUT   C*?  Task Input object
C      UVIN    C*?  Input uv data
C   Output:
C      UVOUT   C*?  Name of output object.
C-----------------------------------------------------------------------
      CHARACTER INPUT*(*), UVIN*(*), UVOUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=5)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'NUMBIF'/
C-----------------------------------------------------------------------
C                                       Copy old history
      CALL OHCOPY (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       New additions - copy adverb
C                                       values.
      CALL OHLIST (INPUT, LIST, NADV, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // UVOUT
      CALL MSGWRT (6)
C
 999  RETURN
      END

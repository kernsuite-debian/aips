LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NPARMS
      PARAMETER (NPARMS=34)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'SOURCES', 'QUAL', 'CALCODE', 'STOKES', 'TIMERANG',
     *   'SELBAND', 'SELFREQ', 'FREQID', 'SUBARRAY', 'BIF',
     *   'EIF', 'BCHAN', 'ECHAN', 'DOCALIB', 'GAINUSE', 'DOPOL',
     *   'PDVER', 'BLVER', 'FLAGVER', 'DOBAND', 'BPVER', 'SMOOTH',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'CALSOUR', 'REFDATE', 'DOUVCOMP', 'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOAINT, OOACAR, OOACAR, OOARE,
     *   OOARE, OOARE, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOARE, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOARE,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOALOG, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   16,30, 1,1, 4,1, 4,1, 8,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 3,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   16,30, 8,1, 1,1, 10,1/
LOCAL END
      PROGRAM UV2MS
C-----------------------------------------------------------------------
C! Append single source file to multisource file.
C# Task UV OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004, 2008, 2010, 2012
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
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA PRGM /'UV2MS'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL UV2MIN (PRGM, INPUT, UVIN, UVOUT, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       SoUrce table
      CALL UV2MSU (INPUT, UVIN, UVOUT, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Append
      CALL UV2MCP (UVIN, UVOUT, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       History
      CALL UV2MHI (INPUT, UVIN, UVOUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UV2MIN (PRGN, INPUT, UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UV2MIN gets input parameters for UV2MS and creates the input and
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
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY1=25)
C                                       No. abverbs for UVOUT
      PARAMETER (NKEY2=5)
      INTEGER   DIM(7), TYPE, INDXSU, NASAVE, SEQO, SEQI, NRPARM, JERR,
     *   NRPI, NRPO, NCI, NCO, DUMMY, IY
      LOGICAL   EXIST, LTEMP
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, PTYPE(20)*8, NAMEI*12, NAMEO*12, CLASSI*6,
     *   CLASSO*6, SOURCE(30)*16, OBJECT*8, CDUMMY*1, REFDAY*8, STR*8
      REAL      XDOCAL
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVIN
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'SOURCES', 'QUAL', 'CALCODE',
     *   'STOKES', 'TIMERANG', 'SELBAND',
     *   'SELFREQ', 'FREQID', 'SUBARRAY',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN',
     *   'GAINUSE', 'DOPOL', 'BLVER',
     *   'FLAGVER', 'DOBAND', 'BPVER',
     *   'SMOOTH', 'PDVER'/
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK',
     *   'CALEDIT.SOURCS', 'CALEDIT.SELQUA', 'CALEDIT.SELCOD',
     *   'CALEDIT.STOKES', 'CALEDIT.TIMRNG','CALEDIT.SELBAN',
     *   'CALEDIT.SELFRQ', 'CALEDIT.FRQSEL', 'CALEDIT.SUBARR',
     *   'CALEDIT.BIF', 'CALEDIT.EIF', 'CALEDIT.BCHAN', 'CALEDIT.ECHAN',
     *   'CALEDIT.CLUSE', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
     *   'CALEDIT.FGVER', 'CALEDIT.DOBAND', 'CALEDIT.BPVER',
     *   'CALEDIT.SMOOTH', 'CALEDIT.PDVER'/
C                                       Adverbs for UVOUT
      DATA INK2 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'DOUVCOMP'/
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'ISCOMP'/
C-----------------------------------------------------------------------
C                                       Startup
      INPUT = 'Task Input'
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, INPUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET (INPUT, 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reference day
      CALL OGET (INPUT, 'REFDATE', TYPE, DIM, DUMMY, REFDAY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create UVIN
      UVIN = 'Input UVdata'
      CALL CREATE (UVIN, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY1, INK1, OUTK1, UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET (INPUT, 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVIN, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVIN to be sure it's OK.
      CALL OOPEN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Must be single source file
      CALL UVDFND (UVIN, 1, 'SOURCE', INDXSU, IERR)
      IF ((IERR.EQ.0) .AND. (INDXSU.GT.0)) THEN
         IERR = 2
         MSGTXT = 'INPUT MUST SELECT A SINGLE SOURCE'
         GO TO 990
         END IF
C                                       Get vis size
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCI, CDUMMY, IERR)
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
C                                       Source name
      CALL OGET (INPUT, 'CALSOUR', TYPE, DIM, DUMMY, SOURCE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (SOURCE(1).EQ.' ') CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM,
     *   DUMMY, SOURCE(1), IERR)
      IF ((SOURCE(1).EQ.' ') .OR. (SOURCE(1).EQ.'MULTI'))
     *   SOURCE(1) = NAMEI
C                                       Save for history
      DIM(1) = 16
      DIM(2) = 30
      DIM(3) = 1
      CALL OPUT (INPUT, 'CALSOUR', OOACAR, DIM, DUMMY, SOURCE, IERR)
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
C                                       Does output exist?
      CALL OBFEXS (UVOUT, EXIST, JERR)
      EXIST = EXIST .AND. (JERR.EQ.0)
C                                       If output doesn't exist add
C                                       SOURCE random parameter.
      IF (.NOT.EXIST) THEN
C                                       Set reference day
         IF (REFDAY.NE.'        ') THEN
C                                       change old format
            IF (REFDAY(3:3).EQ.'/') THEN
               STR = REFDAY
               REFDAY(7:8) = STR(1:2)
               REFDAY(5:6) = STR(4:5)
               REFDAY(3:4) = STR(7:8)
               READ (STR,1020) IY
               IF (IY.LE.40) THEN
                  REFDAY(1:2) = '20'
               ELSE
                  REFDAY(1:2) = '19'
                  END IF
               END IF
            DIM(1) = 8
            DIM(2) = 1
            CALL UVDPUT (UVOUT, 'DATE-OBS', OOACAR, DIM, DUMMY, REFDAY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Number of random parameters.
         CALL UVDGET (UVOUT, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Names
         CALL UVDGET (UVOUT, 'PTYPE', TYPE, DIM, DUMMY, PTYPE, IERR)
         IF (IERR.NE.0) GO TO 999
         NASAVE = DIM(2)
C                                       Check for source random
C                                       parameter
         CALL UVDFND (UVOUT, 1, 'SOURCE', INDXSU, IERR)
C                                       If not ther add to end
         IF ((IERR.NE.0) .OR. (INDXSU.LE.0)) THEN
            IERR = 0
            NRPARM = NRPARM + 1
            INDXSU = NRPARM
            PTYPE(INDXSU) = 'SOURCE'
            END IF
C                                       Save random parameter info
         DIM(1) = 1
         DIM(2) = 1
         CALL UVDPUT (UVOUT, 'NRPARM', OOAINT, DIM, NRPARM, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = LEN (PTYPE(1))
         DIM(2) = NASAVE
         CALL UVDPUT (UVOUT, 'PTYPE', OOACAR, DIM, DUMMY, PTYPE, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reset "OBJECT"
         OBJECT = 'MULTI'
         DIM(1) = LEN (OBJECT)
         DIM(2) = 1
         CALL UVDPUT (UVOUT, 'OBJECT', OOACAR, DIM, DUMMY, OBJECT,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Open and close to force create
         CALL OOPEN (UVOUT, 'WRITE', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Fudge descriptor to reflect
C                                       uncompressed data (proper
C                                       state will be restored on close)
         CALL OUVPAK (UVOUT, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get vis size
         CALL UVDGET (UVOUT, 'NRPARM', TYPE, DIM, NRPO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDGET (UVOUT, 'NCORR', TYPE, DIM, NCO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OCLOSE (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy relevant tables if output
C                                       just created.
         CALL UVDTCO (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       If OLD open read to check
      ELSE
         CALL OOPEN (UVOUT, 'READ', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get vis size
         CALL UVDGET (UVOUT, 'NRPARM', TYPE, DIM, NRPO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDGET (UVOUT, 'NCORR', TYPE, DIM, NCO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OCLOSE (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Crude check for compatability
      IF ((NRPO.NE.(NRPI+1)) .OR. (NCO.NE.NCI)) THEN
         IERR = 2
         MSGTXT = 'VISIBILITY GEOMETRIES ARE INCOMPATIBLE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (6X,I2)
      END
      SUBROUTINE UV2MSU (INPUT, UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   Add to source table and attach new source id  ('SOUR_ID') and
C   timeoffset ('TIME_OFF') to UVIN
C   Inputs:
C      INPUT   C*?  Task inputs
C      UVIN    C*?  Input file.
C      UVOUT   C*?  Output file.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INPUT*(*), UVIN*(*), UVOUT*(*)
      INTEGER   IERR
      INTEGER DIR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SUTAB*32, SOURCS(30)*16, VELTYP*8, VELDEF*8, SOUNAM*16,
     *   CALCOD*4, DATOBS*8, CDUMMY*1
      INTEGER  DIM(3), TYPE, SUROW, NUMIF, FREQID, LOOP, NROW, IDSOU,
     *   QUAL, INDXIF, INDXF, INDXRA, INDXDC, NAXIS(7), VELREF, DUMMY
      REAL      FLUX(4,MAXIF), CDELT(7), EPO, TIMOFF, POLAR(2)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC, CRVALI(7),
     *   CRVALO(7), RRMEA, RDMEA, DEG2RD, DELDAT, EQUIN, RRAPP, RDAPP,
     *   JD, JDO, OBSPOS(3), RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OBSPOS, POLAR /3 * 0.0D0, 2 * 0.0/
C-----------------------------------------------------------------------
C                                       New source name
      CALL OGET (INPUT, 'CALSOUR', TYPE, DIM, DUMMY, SOURCS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Other source info
      CALL UVDGET (UVOUT, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UVIN, 2, 'IF  ', INDXIF, IERR)
C                                       Number of IFs
      IF ((IERR.EQ.0) .AND. (INDXIF.GT.0)) THEN
         NUMIF = NAXIS(INDXIF)
      ELSE
         NUMIF = 1
         END IF
      CALL UVDFND (UVIN, 2, 'FREQ', INDXF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING FREQUENCY AXIS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Velocity info
      CALL VELGET (UVIN, 'VELREF', TYPE, DIM, VELREF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELGET (UVIN, 'RESTFREQ', TYPE, DIM, LRESTF(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (VELREF.GT.256) THEN
         VELDEF = 'RADIO'
         VELREF = VELREF - 256
      ELSE
         VELDEF = 'OPTICAL'
         END IF
      VELTYP = '        '
      IF (VELREF.EQ.1) VELTYP = 'LSR'
      IF (VELREF.EQ.2) VELTYP = 'BARYCENT'
      IF (VELREF.EQ.3) VELTYP = 'TOPOCENT'
      DO 50 LOOP = 1,MAXIF
         LSRVEL(LOOP) = 0.0D0
         LRESTF(LOOP) = LRESTF(1)
 50      CONTINUE
C                                        Time offset
C                                        Calculate Julian date
      CALL UVDGET (UVIN, 'DATE-OBS', TYPE, DIM, DUMMY, DATOBS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL JULDAY (DATOBS, JD)
      CALL UVDGET (UVOUT, 'DATE-OBS', TYPE, DIM, DUMMY, DATOBS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL JULDAY (DATOBS, JDO)
      TIMOFF = JD - JDO
C                                       Disallow backwards time jumps
      IF (TIMOFF.LT.-0.1) THEN
         IERR = 2
         MSGTXT = 'INPUT DATA BEFORE OUTPUT REFERENCE DATE '
         GO TO 985
         END IF
C                                       Make table object from UVOUT
      SUTAB = 'Temporary SU table for UV2MSU'
      CALL UV2TAB (UVOUT, SUTAB, 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open/create table
      FREQID = 1
      CALL OSUINI (SUTAB, 'WRIT', NUMIF, VELTYP, VELDEF, FREQID,
     *   SUROW, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (SUTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Look to see if source is already
C                                       there.
      DO 100 LOOP = 1,NROW
         SUROW = LOOP
         CALL OTABSU (SUTAB, 'READ', SUROW, IDSOU, SOUNAM, QUAL, CALCOD,
     *      FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *      RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Found it?
         IF ((SOUNAM.EQ.SOURCS(1)) .AND. (QUAL.EQ.0)) GO TO 500
 100     CONTINUE
C                                       Make new entry
      IDSOU = NROW + 1
      SOUNAM = SOURCS(1)
      QUAL = 0
      CALL UVDGET (UVIN, 'CRVAL', TYPE, DIM, CRVALI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVOUT, 'CRVAL', TYPE, DIM, CRVALO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVIN, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      FREQO(1) = CRVALI(INDXF) - CRVALO(INDXF)
      DO 150 LOOP = 2,MAXIF
         FREQO(LOOP) = FREQO(1)
 150     CONTINUE
      BANDW = CDELT(INDXF)
      CALCOD = '    '
      CALL RFILL (4*MAXIF, 0.0, FLUX)
C                                       Positions
      CALL UVDFND (UVIN, 2, 'RA', INDXRA, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RA AXIS'
         GO TO 990
         END IF
      CALL UVDFND (UVIN, 2, 'DEC', INDXDC, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING DEC AXIS'
         GO TO 990
         END IF
      RAEPO = CRVALI(INDXRA)
      DECEPO = CRVALI(INDXDC)
      CALL UVDGET (UVIN, 'EPOCH', TYPE, DIM, EPO, CDUMMY, IERR)
      EPOCH = EPO
      IF (IERR.NE.0) GO TO 999
C                                       pointing position
      CALL OUVCGT (UVIN, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'WARNING: TROUBLE READING INPUT HEADER'
         CALL MSGWRT (6)
         RAOBS = RAEPO
         DECOBS = DECEPO
      ELSE
         RAOBS = CATD(KDORA)
         DECOBS = CATD(KDODE)
         END IF
C                                        Convert degrees to radians,
C                                        precess coordinates, and
C                                        convert back to degrees
      DEG2RD = ATAN (1.0D0) / 45.
      RRMEA  =  RAEPO * DEG2RD
      RDMEA  = DECEPO * DEG2RD
      EQUIN = EPOCH
      DELDAT = 0.1
      DIR = 1
C
      CALL JPRECS (JD, EQUIN, DELDAT, DIR, .FALSE., OBSPOS, POLAR,
     *   RRMEA, RDMEA, RRAPP, RDAPP)
C
C      CALL PRECES (JD, EQUIN, DELDAT, RRMEA, RDMEA, RRAPP, RDAPP,
C     *   .TRUE., .FALSE., .FALSE., 0.0D0, 0.0D0, 0.0D0, .FALSE.)
      RAAPP  = RRAPP / DEG2RD
      DECAPP = RDAPP / DEG2RD
      PMRA = 0.0D0
      PMDEC = 0.0D0
C                                       Update table
      CALL OTABSU (SUTAB, 'WRIT', SUROW, IDSOU, SOUNAM, QUAL, CALCOD,
     *   FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close table
 500  CALL OTABSU (SUTAB, 'CLOS', SUROW, IDSOU, SOUNAM, QUAL, CALCOD,
     *   FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set source ID on UVIN
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVIN, 'SOUR_ID', OOAINT, DIM, IDSOU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set time offset on UVIN
      CALL OPUT (UVIN, 'TIME_OFF', OOARE, DIM, TIMOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary object
      CALL OBFREE (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'ERROR UPDATING SU TABLE FOR ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UV2MCP (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   Appends UVIN to end of UVOUT.
C   Source numbers are added and the time label is modified to reflect
C   the output file reference date.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C                   Needs to have been opened (and closed) prior to call
C      UVOUT   C*?  Name of output uvdata object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   COUNT, GCOUNT, TYPE, DIM(7), IDSOU, INDXSU, INDXT, DUMMY
      CHARACTER SORD*2, CDUMMY*1
      INCLUDE 'INCS:PUVD.INC'
      REAL      RP(50), VS(3,MAXCIF), RSUID, TIMOFF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set output vis offset
      CALL UVDGET (UVOUT, 'GCOUNT', TYPE, DIM, GCOUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'VISOFF', OOAINT, DIM, GCOUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output, write at end
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find SOURCE random parameter
      CALL UVDFND (UVOUT, 1, 'SOURCE', INDXSU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING SOURCE RANDOM PARAMETER'
         GO TO 990
         END IF
C                                       Find TIME1 random parameter
      CALL UVDFND (UVOUT, 1, 'TIME1', INDXT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING TIME1 RANDOM PARAMETER'
         GO TO 990
         END IF
C                                       New source ID
      CALL OGET (UVIN, 'SOUR_ID', TYPE, DIM, IDSOU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      RSUID = IDSOU
C                                       Time offset
      CALL OGET (UVIN, 'TIME_OFF', TYPE, DIM, TIMOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Init vis array
      CALL RFILL (3*MAXCIF, 0.0, VS)
C                                       Initialize visibility count
      COUNT = 0
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
         COUNT = COUNT + 1
C                                       Source ID random parameter
         RP(INDXSU) = RSUID
C                                       Modify time wrt output ref.
C                                       date.
         RP(INDXT) = RP(INDXT) + TIMOFF
C                                       Write to output
         CALL UVWRIT (UVOUT, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 990
         GO TO 100
 200     IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UV2MCP: NO DATA SELECTED'
         GO TO 985
         END IF
C                                       Mark output as unsorted
      SORD = '  '
      DIM(1) = LEN (SORD)
      DIM(2) = 1
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'UV2MCP: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UV2MHI (INPUT, UVIN, UVOUT)
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
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'SOURCES'/
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
 999  RETURN
      END

LOCAL INCLUDE 'VLAGN.INC'
C                                       include for VLA gains
      INTEGER   MXANT, MXFQID, NOBAND
      PARAMETER (MXANT = 29)
      PARAMETER (MXFQID = 50)
      PARAMETER (NOBAND = 10)
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   OPACIT, GNTYP, MJD, IBANDF(MXFQID), WXROW, NWXROW, NIF,
     *   NTFAIL(2), NFGAIN(NOBAND)
      REAL      ZOPAC, WTOPAC, AGAINS(4,MXANT+1,NOBAND,6), TS1, TS2,
     *   TD1, TD2, PR1, PR2
      DOUBLE PRECISION TIME1, TIME2, FREQS(MAXIF,MXFQID),
     *   FGAINS(NOBAND,6)
      COMMON /VLAGNS/ TIME1, TIME2, FREQS, FGAINS, AGAINS, NFGAIN,
     *   ZOPAC, WTOPAC, OPACIT, GNTYP, MJD, IBANDF, TS1, TS2, TD1, TD2,
     *   PR1, PR2, WXROW, NWXROW, NIF, NTFAIL
LOCAL END
LOCAL INCLUDE 'IMMCTAB.INC'
      INTEGER   NUMIMR, NUMMCR
      REAL      MCDELT
      COMMON /IMCOM/ MCDELT, NUMIMR, NUMMCR
LOCAL END
LOCAL INCLUDE 'INDXR.INC'
      LOGICAL   ADAPT, EVLA
      COMMON /INDXRC/ ADAPT, EVLA
LOCAL END
      PROGRAM INDXR
C-----------------------------------------------------------------------
C! Indexes a multisource uv data file.
C# UV Calibration EXT-appl OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-2003, 2007-2016
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
C   Index a time-ordered multi source data file and optionally
C   generate a default calibration (CL or CS) table.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      INFILE.....File containing times at which to force scan breaks.
C      PRTLEV.....Echo INFILE if > 0.
C      CPARM......1=max. gap in scan (min.)
C                 2=max. scan length (min.)
C                 3=CL/CS entry interval,
C                 4=recalc CL group delays from IM table (VLBA only)?
C                 5=recalc CL clock offsets and atmospheric delays
C                   from MC table (VLBA only)?
C                 6=max. antenna number if AN tables are missing
C-----------------------------------------------------------------------
C   Local variables:
C
C   IRET      I          Status code (0 implies everything is OK)
C   INPUTS    C*32       Name of INPUTS object holding adverb values
C   UVDATA    C*32       Name of UVDATA object
C   INDEX     C*32       Name of TABLE object for new NX table
C   CALTAB    C*32       Name of TABLE object for new CL or CS table:
C                        blank if no new calibartion table is being
C                        generated
C   IMTAB     C*32       Name of TABLE object for existing IM table:
C                        blank if IM table will not be used
C   MCTAB     C*32       Name of TABLE object for existing MC table:
C                        blank if MC table will not be used
C   SCRTCH    I(256)     Scratch buffer
C
      INTEGER   IRET, SCRTCH(256)
      CHARACTER INPUTS*32, UVDATA*32, INDEX*32, CALTAB*32, IMTAB*32,
     *   MCTAB*32, WXTAB*32
      REAL      X
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'VLAGN.INC'
C-----------------------------------------------------------------------
C                                       init
      CALL ININXR (INPUTS, UVDATA, INDEX, CALTAB, IMTAB, MCTAB, WXTAB,
     *   IRET)
C                                       run
      IF (IRET.EQ.0) CALL RUNNXR (UVDATA, IRET)
C                                       clean up
      IF (IRET.EQ.0) CALL FINNXR (INPUTS, UVDATA, INDEX, CALTAB, IMTAB,
     *   MCTAB, WXTAB, IRET)
      IF ((NTFAIL(1).GT.0) .AND. (NTFAIL(2).GT.0)) THEN
         X = (100.0 * NTFAIL(2)) / NTFAIL(1)
         WRITE (MSGTXT,1000) X
         CALL MSGWRT (7)
         MSGTXT = 'This may be a normal amount'
         IF (X.LT.5.0) CALL MSGWRT (7)
         END IF
C                                       close down
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Warning:',F8.3,' percent of CL times not found in',
     *   ' weather table')
      END
      SUBROUTINE ININXR (INPUTS, UVDATA, INDEX, CALTAB, IMTAB, MCTAB,
     *   WXTAB, IRET)
C-----------------------------------------------------------------------
C   Initialize the INDXR task.  Read input adverbs and substitute
C   default values where the inputs are out of normal ranges.  Check
C   that the preconditions for generating a new index table are met (ie.
C   the specified data file exists and contains valid, time-sorted
C   data).  Acquire a write lock on the data file and open it for
C   reading.  Open any tables required by the application, creating them
C   if required.  Initialize the scan manager which is responsible for
C   maintaining the scan definitions.
C   Outputs:
C      INPUTS   C*(*)   The name of an INPUTS object holding the adverb
C                       values for this instance of the task.  If IRET
C                       is zero the the values of the CPARM array will
C                       have been updated to reflect any substitutions
C                       of default values or any requests that have been
C                       overridden.
C      UVDATA   C*(*)   The name of the UVDATA object associated with
C                       the input file.  If IRET is zero then the
C                       associated file is marked with a write flag, is
C                       in time order and is open for reading at the
C                       first visibility record.
C      INDEX    C*(*)   The name of the TABLE object used to access the
C                       output index table.  If IRET is zero then this
C                       refers to a newly created index table with
C                       version number 1 that is open for writing and
C                       any pre-existing index table has been destroyed.
C      CALTAB   C*(*)   The name of the TABLE object used to access the
C                       output calibration table: blank if no new table
C                       will be generated. If IRET is zero and CALTAB is
C                       not blank then it refers to a newly created
C                       calibration table with version number 1 and that
C                       is open for writing.  This will be a CS table if
C                       the input file contains single disk data and
C                       will be a CL table if the input file contains
C                       interferometry data.
C      IMTAB    C*(*)   The name of the TABLE object used to access the
C                       interferometer model table:  blank if an
C                       interferometer model table will not be used.  If
C                       non-blank this refers to an open, time-sorted IM
C                       table with version number 1.
C      MCTAB    C*(*)   The name of the TABLE object used to access the
C                       model components table: blank if a model
C                       components table will not be used.  If non-blank
C                       this refers to an open, time-sorted MC table
C                       with version number 1.
C      IRET     I       Return status: 0 indicates all OK
C                       anything else indicates an unrecoverable error
C   The name arguments should all be capable of holding at least 16
C   characters.
C
C   If CPARM(1) <= 0.0 then INPUTS.CPARM(1) is set to 10.0
C   If CPARM(2) <= 0.0 then INPUTS.CPARM(2) is set to 60.0
C   CALTAB will only be non-blank if CPARM(3) >= 0.0 and there is not
C      a CL/CS table with version number 1 already.
C   If CALTAB is blank INPUTS.CPARM(3) is set to zero.
C   If CALTAB is not blank and CPARM(3) was 0.0 then INPUTS.CPARM(3)
C      is set to 5.0
C   IMTAB will only be non-blank if CPARM(4) > 0.0, CALTAB is not blank
C      and there is an existing IM table with version number 1.
C   If IMTAB is blank then INPUTS.CPARM(4) is set to 0.0
C   MCTAB will only be non-blank if CPARM(5) > 0.0, CALTAB is not blank
C      and there is an existing MC table with version number 1.
C   If MCTAB is blank then INPUTS.CPARM(5) is set to 0.0
C
C   IRET will not be zero if the file does not exist, can not be
C      set to 'WRIT' status or is not in time order.
C   IRET will not be zero if the file has no antenna tables and
C      CPARM(6) <= 0.0
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*), INDEX*(*), CALTAB*(*),
     *   IMTAB*(*), MCTAB*(*), WXTAB*(*)
      INTEGER   IRET
C
C                                       Local variables
C
C     PRGNAM    C*6          Program name (constant)
C
C     NINPUT    I            Number of input adverbs (constant)
C     AVNAME    C*8(NINPUT)  Adverb names (constant)
C     AVTYPE    I(NINPUT)    Adverb type codes (constant)
C     AVDIM     I(2, NINPUT) Adverb array dimensions (constant)
C
C     INFILE    C*48         Auxilliary file name
C     PRTLEV    I            Print level
C
C     CPARM     R(10)        Values of CPARM array adverb
C
C     GAP       R            Maximum time gap in days (GAP > 0.0)
C     LEN       R            Maximum scan length in days (LEN > 0.0)
C     INC       R            Calibration table time increment in days
C                             (INC > 0.0 if CALTAB /= ' ')
C
C     NPOL      I            Number of receiver polarizations in data
C                             (1 <= NPOL <= 2)
C     NIF       I            Number of IFs in data (1 <= NIF <= MAXIF)
C     NSTA      I            Maximum antenna number in data
C                             (1 <= NSTA)
C
C     TYPE      I            Attribute type code
C     DIM       I(3)         Attribute dimensions
C     CDUMMY    C            Dummy character value
C     NDUMMY    I            Dummy numeric value
C
C     NXROW     I            Index table row number
C
      CHARACTER PRGNAM*6
      PARAMETER (PRGNAM = 'INDXR ')
C
      INTEGER   NINPUT
      PARAMETER (NINPUT = 9)
      CHARACTER AVNAME(NINPUT)*8
      INTEGER   AVTYPE(NINPUT), AVDIM(2, NINPUT)
C
      CHARACTER INFILE*48, ANAME*8, IN2FIL*48
      REAL      CPARM(10), GAP, LEN, INC, BPARM(10)
      INTEGER   PRTLEV, NPOL, NIF, NSTA, DISK, CNO, TYPE, DIM(3),
     *   NDUMMY, NXROW
      LOGICAL   MODVLB
      CHARACTER CDUMMY
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INDXR.INC'
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *   'INFILE  ', 'PRTLEV  ', 'CPARM   ', 'BPARM ', 'CALIN'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOAINT,
     *   OOARE, OOARE, OOACAR/
      DATA AVDIM  /12,1,  6,1,  1,1,  1,1,  48,1,  1,1,  10,1,  10,1,
     *   48,1/
C-----------------------------------------------------------------------
C                                       read inputs
      INPUTS = 'inputs'
      CALL AV2INP (PRGNAM, NINPUT, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'BPARM', TYPE, DIM, BPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'CALIN', TYPE, DIM, NDUMMY, IN2FIL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Initialize the UV data file:
      CALL INIUVD (INPUTS, CPARM, UVDATA, NPOL, NIF, NSTA, MODVLB,
     *   ANAME, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       block inappropriate options
      EVLA = (ANAME.EQ.'EVLA')
      IF ((ANAME.EQ.'VLA ') .OR. (ANAME.EQ.'EVLA')) THEN
         CPARM(4) = 0.0
         CPARM(5) = 0.0
         IF (CPARM(6).EQ.0.0) CPARM(6) = 29.0
      ELSE
         BPARM(1) = -1.0
         BPARM(2) = -1.0
         IN2FIL = ' '
         END IF
C                                       The data file exists, is in
C                                       time order, marked with a
C                                       write flag and closed: it can
C                                       not be opened until any existing
C                                       index file has been removed.
C                                       1 <= NPOL <= 2
C                                       1 <= NIF <= MAXIF
C                                       0 <= NSTA
C
C                                       Remove any existing index
C                                       tables:
      INDEX = 'NX table'
      CALL UV2TAB (UVDATA, INDEX, 'NX', -1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABRMV (INDEX, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reset status so that we can
C                                       create tables
      CALL OBDSKC (UVDATA, DISK, CNO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUCFCL (UVDATA, DISK, CNO, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUCCLR (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       NXROW = 1
C                                       longest gap and scan
      ADAPT = CPARM(1).LE.0.0
      GAP = CPARM(1) / (24.0 * 60.0)
      IF (ADAPT) GAP = 10.0 / (60. * 24.0)
      IF (CPARM(2).LE.0.0) CPARM(2) = 60.0
      LEN = CPARM(2) / (24.0 * 60.0)
C
C     Take the maximum antenna number from CPARM(6) if there were
C     no antenna files (indicated by NSTA = 0):
C
      IF (NSTA.GT.0) THEN
         CPARM(6) = 0
      ELSE IF (NINT (CPARM(6)).GT.0) THEN
         NSTA = NINT (CPARM(6))
         CPARM(6) = NSTA
      ELSE
         MSGTXT = 'YOU MUST SET CPARM(6) TO A POSITIVE INTEGER'
         CALL MSGWRT (9)
         MSGTXT = 'IF NO ANTENNA TABLES ARE PRESENT'
         CALL MSGWRT (9)
         IRET = 1
         GO TO 999
         END IF
C
C     Initialize calibration tables depending on the values of the
C     CPARM adverb array:
C
      IF (BPARM(1).EQ.0.0) BPARM(1) = 20.
      IF (BPARM(10).EQ.0.0) BPARM(10) = 0.5
      BPARM(10) = MAX (0.0, MIN (1.0, BPARM(10)))
      WXTAB = ' '
      IF (BPARM(1).GT.10.0) WXTAB = 'WX table'
      IF (CPARM(3).EQ.0.0) CPARM(3) = 5.0
C                                       CPARM(3).NE.0.0
      IF (CPARM(3).GT.0.0) THEN
         CALL INICTB (UVDATA, CPARM, CALTAB, IMTAB, MCTAB, NPOL, NIF,
     *      NSTA, WXTAB, BPARM, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       not doing a CL table
      IF (CPARM(3).LE.0.0) THEN
         CALTAB = ' '
         IMTAB  = ' '
         MCTAB  = ' '
         WXTAB  = ' '
         BPARM(1) = -1.0
         BPARM(2) = -1.0
         END IF
      INC = CPARM(3) / (24.0 * 60.0)
C
C     Create the new index table (this could not be done safely before
C     opening the data file):
C
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (INDEX, 'VER', TYPE, DIM, 1, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ONXINI (INDEX, 'WRIT', NXROW, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Now open the data file:
      CALL OUVOPN (UVDATA, 'RRAW', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Init the VLA gains
      IF ((BPARM(1).GE.0.0) .OR. (BPARM(2).GE.0.0)) THEN
         CALL VLAGIN (UVDATA, WXTAB, BPARM, IN2FIL, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
C     Update INPUTS with the revised values for CPARM so that the
C     history file will record the actual values used rather than
C     the pre-default values:
C
      TYPE = OOARE
      DIM(1) = 10
      DIM(2) = 1
      DIM(3) = 0
      CALL INPUTT (INPUTS, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INPUTT (INPUTS, 'BPARM', TYPE, DIM, BPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TYPE = OOACAR
      DIM(1) = 48
      CALL INPUTT (INPUTS, 'CALIN', TYPE, DIM, NDUMMY, IN2FIL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       input file name, print level
      CALL INGET (INPUTS, 'INFILE', TYPE, DIM, NDUMMY, INFILE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Initialize the scan module:
      CALL INISCN (UVDATA, INFILE, PRTLEV, INDEX, CALTAB, IMTAB, MCTAB,
     *   WXTAB, GAP, LEN, INC, NPOL, NIF, NSTA, MODVLB, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE VLAGIN (UVDATA, WXTAB, BPARM, IN2FIL, IRET)
C-----------------------------------------------------------------------
C   VLAGIN initializes the parameters needed to recompute VLA opacity
C   and antenna gain corrections
C   Inputs:
C      UVDATA   C*(*)   UV data file
C      WXTAB    C*(*)   WX table
C   In/Out:
C      BPARM    R(10)   Control parameters
C      IN2FIL   C*48    Text file of antenna gains
C   Output:
C      IRET     I       Errror code
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), WXTAB*(*), IN2FIL*(*)
      REAL      BPARM(10)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, DIM(7), TYPE, DUMMY, IBAND, IANT, II, LUNGN,
     *   INDGN, NFQROW, FQROW, FQID, IFSIDE(MAXIF), R, NUMIF, NFG, JT,
     *   JTRIM
      REAL      GPARMS(4), IFCHW(MAXIF), IFTBW(MAXIF)
      CHARACTER DATOBS*8, LINE*132, CDUMMY*1, FQTAB*8, OBSCOD*8,
     *   OBSDAT*8, BNDCOD(MAXIF)*8, CHTEMP*8
      DOUBLE PRECISION JD, VDATE(2), RFREQ, TMPFRQ(MAXIF), FF
      INCLUDE 'VLAGN.INC'
      REAL      DBGAI1(4,MXANT+1), DBGAI2(4,MXANT+1), DBGAI3(4,MXANT+1),
     *   DBGAI4(4,MXANT+1), DBGAI5(4,MXANT+1)
      INCLUDE 'INDXR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DBGAI1, AGAINS(1,1,8,1))
      EQUIVALENCE (DBGAI2, AGAINS(1,1,8,2))
      EQUIVALENCE (DBGAI3, AGAINS(1,1,8,3))
      EQUIVALENCE (DBGAI4, AGAINS(1,1,8,4))
      EQUIVALENCE (DBGAI5, AGAINS(1,1,8,5))
C-----------------------------------------------------------------------
      NTFAIL(1) = 0
      NTFAIL(2) = 0
C                                       Opacity parameters
      IF (BPARM(1).LT.0.0) THEN
         OPACIT = 0
         MSGTXT = 'No opacity correction in CL table.'
      ELSE IF (BPARM(1).LE.10.0) THEN
         OPACIT = 1
         ZOPAC = BPARM(1)
         WRITE (MSGTXT,1000) ZOPAC
      ELSE IF (BPARM(10).GT.0.0) THEN
         OPACIT = 2
         WTOPAC = MIN (1.0, MAX (0.0, BPARM(10)))
         MSGTXT = 'Opacity correction in CL table weighted average of'//
     *            ' weather and'
         CALL MSGWRT (3)
         WRITE (MSGTXT,1005) WTOPAC
      ELSE
         OPACIT = 3
         WTOPAC = 0.0
         MSGTXT = 'Opacity correction in CL table based on seasonal'
     *      // ' average'
         END IF
      IF (BPARM(1).GE.100.0) THEN
         CALL MSGWRT (3)
         OPACIT =  OPACIT + 2
         MSGTXT = 'Old opacity model is being used'
         END IF
      CALL MSGWRT (3)
C                                       gain parameters
C                                       init as zero
      I = NOBAND * 4 * (MXANT+1) * 6
      CALL RFILL (I, 0.0, AGAINS)
      CALL FILL (NOBAND, 0, NFGAIN)
      I = 4 * NOBAND
      CALL DFILL (I, 0.0D0, FGAINS)
C                                       test BPARM
      IF (BPARM(2).LT.0.0) THEN
         GNTYP = 0
         MSGTXT = 'No gain curve correction in CL table.'
      ELSE IF (BPARM(2).GE.3.0) THEN
         GNTYP = 3
         DO 10 I = 1,NOBAND
            DO 10 J = 1,4
               AGAINS(J,MXANT+1,I,1) = BPARM(J+2)
 10            CONTINUE
         MSGTXT = 'Gain curve correction in CL table based on ' //
     *            'user specified: '
         CALL MSGWRT (3)
         WRITE (MSGTXT,1010) (BPARM(J), J = 3,6)
      ELSE IF (BPARM(2).GE.2.0) THEN
         GNTYP = 2
         MSGTXT = 'Gain curve correction in CL table read from file,'
         CALL MSGWRT (3)
         MSGTXT = '   with variation as function of band only.'
      ELSE
         GNTYP = 1
         MSGTXT = 'Gain curve correction in CL table read from file,'
         CALL MSGWRT (3)
         MSGTXT = '   with variation as function of antenna and band.'
         END IF
      CALL MSGWRT (3)
C                                       open WX table
      IF ((OPACIT.EQ.2) .OR. (OPACIT.EQ.4)) THEN
         I = 1
         CALL OWXINI (WXTAB, 'READ', WXROW, OBSCOD, OBSDAT, I, IRET)
         IF (IRET.EQ.0) CALL TABGET (WXTAB, 'NROW', TYPE, DIM, NWXROW,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR OPENING WEATHER TABLE: USING SEASONAL ONLY'
            CALL MSGWRT (7)
            OPACIT = OPACIT + 1
            WTOPAC = 0.0
            END IF
         END IF
      TIME1 = -1.E5
      TIME2 = -1.E5
C                                       get date
      CALL UVDGET (UVDATA, 'DATE-OBS', TYPE, DIM, DUMMY, DATOBS, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL JULDAY (DATOBS, JD)
      MJD = (JD - 2400000.5D0) + 0.00001D0
      LUNGN = 10
C                                       Try user file
      IF ((GNTYP.EQ.1) .OR. (GNTYP.EQ.2)) THEN
         IF (EVLA) THEN
            MSGTXT = 'Trying to access EVLA antenna gains file...'
         ELSE
            MSGTXT = 'Trying to access VLA antenna gains file...'
            END IF
         CALL MSGWRT (3)
         NFG = 1
         IF (IN2FIL.NE.' ') THEN
            CALL ZTXOPN ('READ', LUNGN, INDGN, IN2FIL, .FALSE., IRET)
            IF (IRET.EQ.0) THEN
               CALL ZTXIO ('READ', LUNGN, INDGN, LINE, IRET)
               IF ((IRET.EQ.0) .AND. (LINE(:20).EQ.
     *            '; ANTENNA-GAIN-TABLE')) GO TO 20
               CALL ZTXCLS (LUNGN, INDGN, IRET)
               END IF
            MSGTXT = 'IN2FIL not valid gains file, trying standard one.'
            CALL MSGWRT (6)
            END IF
         IF (EVLA) THEN
            IN2FIL = 'AIPSIONS:EVLA.GAINS'
         ELSE
            IN2FIL = 'AIPSIONS:VLA.GAINS'
            END IF
         CALL ZTXOPN ('READ', LUNGN, INDGN, IN2FIL, .FALSE., IRET)
         IF (IRET.EQ.0) THEN
            CALL ZTXIO ('READ', LUNGN, INDGN, LINE, IRET)
            IF ((IRET.EQ.0) .AND. (LINE(:20).EQ.'; ANTENNA-GAIN-TABLE'))
     *         GO TO 20
            CALL ZTXCLS (LUNGN, INDGN, IRET)
            END IF
         MSGTXT = 'Standard gains file not valid or failed!'
         CALL MSGWRT (6)
         IRET = 1
         GO TO 999
C                                       Continue reading
 20      CALL ZTXIO ('READ', LUNGN, INDGN, LINE, IRET)
         IF (IRET.EQ.0) THEN
            JT = JTRIM (LINE)
            IF (LINE(1:1) .NE. ';') THEN
               CALL PARSEL (LINE, EVLA, IBAND, FF, IANT, GPARMS, VDATE)
               IF ((JD.GE.VDATE(1)) .AND. (JD.LE.VDATE(2)) .AND.
     *            (IANT.GE.0) .AND. (IBAND.GT.0) .AND.
     *            (IBAND.LE.NOBAND)) THEN
                  IF (IANT.EQ.0) IANT = MXANT + 1
                  IF (EVLA) THEN
                     NFG = NFGAIN(IBAND)
                     IF ((NFG.LE.0) .OR.
     *                  (ABS(FF-FGAINS(IBAND,NFG)).GT.1.D6)) THEN
                        NFG = MAX (1, NFG+1)
                        FGAINS(IBAND,NFG) = FF
                        NFGAIN(IBAND) = NFG
                        END IF
                     END IF
                  DO 25 II = 1,4
                     AGAINS(II,IANT,IBAND,NFG) = GPARMS(II)
 25                  CONTINUE
                  END IF
               END IF
            GO TO 20
            END IF
         CALL ZTXCLS (LUNGN, INDGN, II)
         IF (IRET.EQ.2) IRET = 0
         END IF
C                                       get the band codes for each FQID
C                                       Read the reference frequency
      CALL UVDGET (UVDATA, 'REFFREQ', TYPE, DIM, RFREQ, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
C
      FQTAB = 'fq table'
      CALL UV2TAB (UVDATA, FQTAB, 'FQ', 1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OFQINI (FQTAB, 'READ', FQROW, NUMIF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABGET (FQTAB, 'NROW', TYPE, DIM, NFQROW, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 50 R = 1,NFQROW
         CALL OTABFQ (FQTAB, 'READ', FQROW, NUMIF, FQID, TMPFRQ,
     *      IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) GO TO 999
         NIF = NUMIF
         DO 45 I = 1,NUMIF
            FREQS(I,FQID) = RFREQ + TMPFRQ(I)
 45         CONTINUE
C                                       guess band from average freq
         CALL GETBND (NUMIF, FREQS(1,FQID), BNDCOD, IBANDF(FQID),
     *      CHTEMP)
 50      CONTINUE
      CALL OTABFQ (FQTAB, 'CLOS', FQROW, NUMIF, FQID, TMPFRQ,
     *   IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
      IF (IRET .NE. 0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Opacity correction in CL table based on user ',
     *   'specified: ',F5.3)
 1005 FORMAT ('   season.  Weight for weather = ',F4.2)
 1010 FORMAT (4(3X,F5.3))
      END
      SUBROUTINE PARSEL (INLINE, EVLA, IBAND, FF, IANT, GPARMS, VDATE)
C-----------------------------------------------------------------------
C   Parses a line from a gain curve file
C   Input:
C      INLINE   C*       Input line
C      EVLA     L        Is there a frequency in the format
C   Output:
C      IBAND    I        The "band number".
C      FF       D        Frequency
C      IANT     I        Antenna number
C      GPARMS   R(4)     Gain curve polynomial parameters
C      VDATE    D(2)     Julian date range over which these gain curve
C                        coefficients are valid
C-----------------------------------------------------------------------
      CHARACTER INLINE*(*)
      LOGICAL   EVLA
      INTEGER   IBAND, IANT
      REAL      GPARMS(4)
      DOUBLE PRECISION FF, VDATE(2)
C
      INTEGER   II, JJ, NL, JTRIM
      CHARACTER CBAND(10)*1
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
C
      DATA CBAND / '4', 'P', 'L', 'S', 'C', 'X', 'U', 'K', 'A', 'Q'/
C-----------------------------------------------------------------------
C                                       get band
      II = 1
      IBAND = 0
      IANT = -1
      NL = JTRIM (INLINE)
      IF (NL.LE.0) GO TO 999
C                                       Match band
      DO 25 JJ = 1,10
         IF (INLINE(:1).EQ.CBAND(JJ)) IBAND = JJ
 25      CONTINUE
C                                       get freq
      II = 3
      IF (EVLA) THEN
         CALL GETNUM (INLINE, 80, II, X)
         IF (X.EQ.DBLANK) GO TO 999
         FF = X * 1.D6
         END IF
C                                       get antenna
      CALL GETNUM (INLINE, 80, II, X)
      IF (X.EQ.DBLANK) GO TO 999
      IANT = X + 0.01D0
C                                       get dates
      II = II + 1
      JJ = II
      DO 30 II = JJ,NL
         IF (INLINE(II:II).NE.' ') GO TO 35
 30      CONTINUE
      VDATE(1) = 1.D10
      GO TO 999
 35   CALL JULDAY (INLINE(II:II+7), VDATE(1))
      II = II + 9
      JJ = II
      DO 40 II = JJ,NL
         IF (INLINE(II:II).NE.' ') GO TO 45
 40      CONTINUE
      VDATE(2) = -1.D10
      GO TO 999
 45   CALL JULDAY (INLINE(II:II+7), VDATE(2))
      II = II + 9
C                                       get parms
      DO 50 JJ = 1,4
         CALL GETNUM (INLINE, 80, II, X)
         IF (X.NE.DBLANK) GPARMS(JJ) = X
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RUNNXR (UVDATA, IRET)
C-----------------------------------------------------------------------
C   Scan file and update index table and, if requested, calibration
C   table.
C   Input:
C      UVDATA   C*(*)   Name of UVDATA object: file must be open for
C                       reading at first visibility record.
C   Output:
C      IRET     I       Status: 0 indicates file scanned
C                       anything else indicates an unrecoverable error
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      INTEGER   IRET
C                                       Local variables
C     MAXRP     I         Maximum number of random parameters (constant)
C     RP        R(MAXRP)  Random parameter array
C     VIS       R(3,MAXCIF)  Visibility buffer (ignored)
C     ILOCU     I         Index of U/RA random parameter
C     ILOCV     I         Index of V/dec random parameter
C     ILOCW     I         Index of W random parameter
C     ILOCT     I         Index of time random parameter
C     ILOCB     I         Index of baseline random parameter
C     ILOCSU    I         Index of source random parameter or 0 if
C                          this parameter is missing
C     ILOCFQ    I         Index of frequency-group random parameter or
C                          0 if this parameter is missing
C     ILOCSC    I         Index of scan random parameter or 0 if this
C                          parameter is missing
C     JLOCC     I         Ordinal number of COMPLEX axis
C     JLOCS     I         Ordinal number of STOKES axis
C     JLOCF     I         Ordinal number of FREQUENCY axis
C     JLOCR     I         Ordinal number of RA axis
C     JLOCD     I         Ordinal number of DEC axis
C     JLOCIF    I         Ordinal number of IF axis
C     INCS      I         STOKES axis increment
C     INCF      I         FREQUENCY axis increment
C     INCIF     I         IF axis increment
C
C     SRC       I         current source number (defaults to 1)
C     SCN       I         current scan number (defaults to 1)
C     FQID      I         current FQID number (defaults to 1)
C
      INTEGER   MAXRP
      PARAMETER (MAXRP = 14)
      INCLUDE 'INCS:PUVD.INC'
      REAL      RP(MAXRP), VIS(3 * MAXCIF)
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA, ILOCSC, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, SRC, SCN, FQID, A1, A2,
     *   CURSUB
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       pointers into random-parameters
      CALL UVDPNT (UVDATA, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVDATA, 1, 'SCAN', ILOCSC, IRET)
      IF (IRET.EQ.1) IRET = 0
      IF (IRET.NE.0) GO TO 999
C
C     Check that data is multi-source (index files can cause problems
C     when single-source data is copied):
C
      IF (ILOCSU.LT.1) THEN
C         MSGTXT = 'INDXR SHOULD ONLY BE RUN ON MULTI-SOURCE FILES'
C         CALL MSGWRT (9)
C         IRET = 1
C         GO TO 999
         END IF
C                                       default random-parameters
      SRC = 1
      SCN = 1
      FQID = 1
C
C     Loop over all data in file and pass it to the scan-manager which
C     determines when index table and calibration table records must be
C     written:
C
 10   CALL UVREAD (UVDATA, RP, VIS, IRET)
      IF (IRET.EQ.0) THEN
         IF (ILOCSU.GT.0) SRC = NINT (RP(ILOCSU))
         IF (ILOCSC.GT.0) SCN = NINT (RP(ILOCSC))
         IF (ILOCFQ.GT.0) FQID = NINT (RP(ILOCFQ))
         IF (ILOCB.GT.0) THEN
            A2 = RP(ILOCB) + 0.01
            A1 = A2 / 256
            CURSUB = (RP(ILOCB) - A2) * 100.0 + 1.5
            A2 = A2 - 256*A1
         ELSE
            A1 = RP(ILOCA1) + 0.1
            A2 = RP(ILOCA2) + 0.1
            CURSUB = RP(ILOCSA) + 0.1
            END IF
         CALL CHKSCN (RP(ILOCT), A1, A2, CURSUB, SRC, SCN, FQID,
     *      RP(ILOCU), RP(ILOCV), IRET)
         IF (IRET.GT.0) GO TO 999
         GO TO 10
         END IF
C                                       IRET < 0 indicates end-of-file
      IRET = MAX (0, IRET)
C
  999 RETURN
      END
      SUBROUTINE FINNXR (INPUTS, UVDATA, INDEX, CALTAB, IMTAB, MCTAB,
     *   WXTAB, IRET)
C-----------------------------------------------------------------------
C   Finalize INDXR.  Write out any open scans. Sort newly created tables
C   into time-subarray order. Close all files and release the lock on
C   the data file.  Update file history.
C   Inputs:
C      INPUTS   C*(*)   Name of INPUTS object with adverb values
C      UVDATA   C*(*)   Name of UVDATA file which should have a 'WRIT'
C                       flag set and be open for reading.
C      INDEX    C*(*)   Name of TABLE object used to access index table,
C                       which should be open.
C      CALTAB   C*(*)   Name of TABLE object used to access cal. table:
C                       blank if cal. table is not generated.  If not
C                       blank, table should be open.
C      IMTAB    C*(*)   Name of TABLE object used to access IM table:
C                       blank if IM table is not generated.  If not
C                       blank, table should be open.
C      MCTAB    C*(*)   Name of TABLE object used to access MC table:
C                       blank if MC table is not generated.  If not
C                       blank, table should be open.
C   Output:
C      IRET     I       Return status: zero indicates finalized
C                       anything else indicates unrecoverable error
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*), INDEX*(*), CALTAB*(*),
     *   IMTAB*(*), MCTAB*(*), WXTAB*(*)
      INTEGER   IRET
C                                       Local variables
C     NINPUT    I             Number of task adverbs (constant)
C     AVNAME    C(NINPUT)*8   Names of task adverbs
C
C     TYPE      I             Attribute type code
C     DIM       I(3)          Attribute value dimensions
C
      INTEGER   NINPUT
      PARAMETER (NINPUT = 8)
      CHARACTER AVNAME(NINPUT)*8
C
      INTEGER   TYPE, DIM(3)
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *   'INFILE  ', 'CPARM   ', 'BPARM ', 'CALIN'/
C-----------------------------------------------------------------------
C                                       Finalize scan manager
      CALL FINSCN (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       close things
      CALL OUVCLO (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABCLO (INDEX, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (CALTAB.NE.' ') THEN
         CALL TABCLO (CALTAB, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (IMTAB.NE.' ') THEN
            CALL TABCLO (IMTAB, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         IF (MCTAB.NE.' ') THEN
            CALL TABCLO (MCTAB, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         IF (WXTAB.NE.' ') THEN
            CALL TABCLO (WXTAB, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
C
C     Sort index table and calibration table into strict time order
C     (note that sorting must be done when all tables are closed).
C
      CALL TBLSRT (INDEX, 'TIME', 'SUBARRAY', IRET)
      IF (IRET.NE.0) GO TO 999
      IF (CALTAB.NE.'    ') THEN
         CALL TBLSRT (CALTAB, 'TIME', 'SUBARRAY', IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Reset file status:
      TYPE = OOACAR
      DIM(1) = 4
      DIM(2) = 1
      DIM(3) = 0
      CALL OUCCLR (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Update file history:
      CALL OHTIME (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OHLIST (INPUTS, AVNAME, NINPUT, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE INIUVD (INPUTS, CPARM, UVDATA, NPOL, NIF, NSTA, MODVLB,
     *   ANAME, IRET)
C-----------------------------------------------------------------------
C   Initialize the input UV data file.
C   Inputs:
C      INPUTS   C*(*)   Name of INPUTS object.
C   Outputs:
C      UVDATA   C*(*)   Name of UVDATA object.  If IRET is zero then
C                       this file has been determined to exist, have
C                       valid data and be in time-order; the file will
C                       not have been opened.
C      NPOL     I       Number of antenna polarizations in data
C                          (1 <= NPOL <= 2)
C      NIF      I       Number of IFs in data
C                          (1 <= NIF <= MAXIF)
C      NSTA     I       Maximum number of antennae or beams in any
C                       subarray or zero if this cannot be determined
C                       because there are no antenna tables.
C                           (0 <= NSTA)
C      MODVLB   L       Special VLB subarray mode - all antennas files
C                       are identical, an antenna number may occur in
C                       only one subarray at a time.
C      ANAME    C*8     Array name
C      IRET     I       Status code: zero indicates file was initialized
C                       non-zero indicates an unrecoverable error
C   Error conditions include no uv file matching INNAME, INCLASS etc.
C   and the data not being time-ordered.
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*), ANAME*8
      REAL      CPARM(10)
      INTEGER   NPOL, NIF, NSTA, IRET
      LOGICAL   MODVLB
C                                       Local variables:
C     NKEY        I           Number of keyword values to copy from
C                             INPUTS to UVDATA
C     INKEY       C(NKEY)*8   Keywords to copy from INPUTS
C     OUTKEY      C(NKEY)*16  Keywords to receive values from INPUTS
C
C     VALID       L           Does file contain valid data?
C     SORT        C*2         Sort order of uv data
C     STATUS      C*4         File status
C
C     NAXIS       I(7)        Shape of visibility group array
C     CRVAL       D(7)        Reference coordinates array
C     STKIND      I           STOKES axis index
C     IFIND       I           IF axis index (<= 0 if not present)
C     SRCIDX      I           Source random parameter index
C
C     ANTAB       C*16        Name of antenna table object
C     NSUB        I           Number of subarrays present
C     SUB         I           Subarray number
C     ACOUNT      I           Number of antennae or beams in subarray
C
C     TYPE        I           Attribute type code
C     DIM         I(3)        Attribute array dimensions
C     NDUMMY      I           Dummy numeric value
C     CDUMMY      C           Dummy character value
C
      INTEGER   NKEY
      PARAMETER (NKEY = 4)
      CHARACTER INKEY(NKEY)*8, OUTKEY(NKEY)*16
C
      LOGICAL   VALID
      CHARACTER SORT*2, STATUS*4, ANTAB*16, CDUMMY*1, DATTYP*2
      INTEGER   NAXIS(7), STKIND, IFIND, SRCIDX, NSUB, SUB, ACOUNT,
     *   TYPE, DIM(3), NDUMMY, DISK, CNO, CATBLK(256)
      DOUBLE PRECISION CRVAL(7)
C                                       antenna file parms
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANROW, NUMORB, NOPCAL, ANFQID
      CHARACTER TIMSYS*8, RDATE*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      INTEGER    NOSTA, MNTSTA, ANTNIF
      CHARACTER  ANNAME*8, POLTYA*2, POLTYB*2
      REAL       STAXOF, DIAMAN, FWHMAN(MAXIF), POLAA, POLCA(2*MAXIF),
     *   POLAB, POLCB(2*MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA INKEY /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OUTKEY /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *   'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
C-----------------------------------------------------------------------
      UVDATA = 'data file'
      CALL OUVCRE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ (INPUTS, NKEY, INKEY, OUTKEY, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check that data is valid:
      CALL FSTGET (UVDATA, 'VALID', TYPE, DIM, VALID, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (.NOT.VALID) THEN
         MSGTXT = 'INPUT FILE DOES NOT CONTAIN VALID DATA'
         CALL MSGWRT (9)
         IRET = 1
         GO TO 999
         END IF
C                                       Check that data is not locked by
C                                       another task:
      CALL FSTGET (UVDATA, 'STATUS', TYPE, DIM, NDUMMY, STATUS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (STATUS.NE.'    ') THEN
         MSGTXT = 'CANNOT PROCEED: INPUT FILE IS LOCKED BY ANOTHER TASK'
         CALL MSGWRT (9)
         IRET = 1
         GO TO 999
         END IF
C                                       Check data is in time order:
      CALL OUVATT (UVDATA, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA, 'SORTORD', TYPE, DIM, NDUMMY, SORT, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SORT(1:1).NE.'T') THEN
         MSGTXT = 'INPUT FILE MUST BE IN TIME-ORDER (USE UVSRT FIRST)'
         CALL MSGWRT (9)
         IRET = 2
         GO TO 999
         END IF
C                                       Check for single-source data:
      CALL UVDFND (UVDATA, 1, 'SOURCE', SRCIDX, IRET)
      IF (IRET.EQ.1) THEN
C         MSGTXT = 'INDXR SHOULD ONLY BE RUN ON MULTI-SOURCE DATA'
C        CALL MSGWRT (10)
C        GO TO 999
         CPARM(3) = -1.0
         IRET = 0
         MSGTXT = 'Making NX table for single-source file, no CL table'
         CALL MSGWRT (6)
      ELSE IF (IRET.NE.0) THEN
         GO TO 999
         END IF
C                                       Find # polarizations and IFs:
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVDATA, 2, 'STOKES', STKIND, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((NAXIS(STKIND).GT.1) .AND. (CRVAL(STKIND).LE.0.0)) THEN
         NPOL = 2
      ELSE
         NPOL = 1
         END IF
      CALL UVDFND (UVDATA, 2, 'IF', IFIND, IRET)
C                                       IF axis present
      IF (IRET.EQ.0) THEN
         NIF = NAXIS (IFIND)
C                                       No IF axis
      ELSE IF (IRET.EQ.1) THEN
         NIF = 1
         IRET = 0
C                                       Some error
      ELSE
         GO TO 999
         END IF
C                                       Find highest number of antennae
C                                       or beams in a subarray:
      CALL UV2TAB (UVDATA, ANTAB, 'AN', 0, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       First get array name
      CALL OANINI (ANTAB, 'READ', NDUMMY, ARRAYC, GSTIA0, DEGPDY,
     *   SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *   TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OTABAN (ANTAB, 'CLOS', ANROW, ANNAME, STAXYZ, ORBPRM, NOSTA,
     *   MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB,
     *   POLAB, POLCB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       now max ant no
      CALL TBLHIV (ANTAB, NSUB, IRET)
      IF (IRET.NE.0) GO TO 999
      NSTA = 0
      DO 10 SUB = 1,NSUB
         CALL ANTNO (ANTAB, SUB, ACOUNT, IRET)
         IF (IRET.NE.0) GO TO 999
         NSTA = MAX (NSTA, ACOUNT)
 10      CONTINUE
      CALL TABDES (ANTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       VLB mode?
      MODVLB = .FALSE.
      IF (NSUB.GT.1) THEN
         CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, NDUMMY, DATTYP,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (DATTYP(:1).EQ.'U') THEN
            CALL OBDSKC (UVDATA, DISK, CNO, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OUVCGT (UVDATA, CATBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ANSAME (DISK, CNO, CATBLK, MODVLB)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE INICTB (UVDATA, CPARM, CALTAB, IMTAB, MCTAB, NPOL, NIF,
     *   NSTA, WXTAB, BPARM, IRET)
C-----------------------------------------------------------------------
C   Initialize calibration tables.
C
C      If CPARM(3) > 0.0 and there is no CL or CS table with version
C   number 1 then a new CL table is created and opened for writing
C      if UVDATA refers to a file containing with interferometry data
C   or an new CS table is created and opened for writing if UVDATA
C   refers to a file containing single dish data.
C      If CPARM(3) < 0.0 or if there is already a CL or CS table with
C   version number 1 then CPARM(3) is set to -1.0 and CALTAB is
C   set to blank.
C      If CALTAB is not blank on output and CPARM(4) > 0.0 on input
C   and there is an interferometer model table with version number
C   1 then IMTAB refers to this table which is open for writing
C    otherwise IMTAB is set to blank and CPARM(4) is set to zero.
C      If CALTAB is not blank on output and CPARM(5) > 0.0 on input
C   and there is an model components table with version number
C   1 then MCTAB refers to this table which is open for writing
C   otherwise MCTAB is set to blank and CPARM(5) is set to zero.
C   Inputs:
C      UVDATA   C*(*)   Name of UVDATA object refering to input data
C                       file (which must exist)
C      NPOL     I       Number of polarizations in data
C                          (1 <= NPOL <= 2)
C      NIF      I       Number of IFs in data
C                          (1 <= NIF <= MAXIF)
C      NSTA     I       Maximum antenna or beam number
C                          (1 <= NSTA)
C   Input/output:
C      CPARM    R(10)   CPARM adverb array; revised to reflect actual
C                       operations to be performed (see above).
C      BPARM    R(10)   Gain, opacity ops
C   Output:
C      CALTAB   C*(*)   Name of TABLE object for CL or CS table; blank
C                       if no CL or CS table will be written
C      IMTAB    C*(*)   Name of TABLE object for IM table; blank if IM
C                       table will not be used
C      MCTAB    C*(*)   Name of TABLE object for MC table; blank if MC
C                       table will not be used
C      IRET     I       Status code: 0 indicates tables initialized
C                       non-zero indicated an unrecoverable error
C   Output names should be long enough to receive a string of up to
C   16 characters
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), CALTAB*(*), IMTAB*(*), MCTAB*(*), WXTAB*(*)
      REAL      CPARM(10), BPARM(10)
      INTEGER   NPOL, NIF, NSTA, IRET
C                                       Local variables
C     DATTYP    C*2         Type code for UV data
C     EXISTS    L           Does table exist?
C     CALROW    I           First row of calibration table
C     NTERM     I           Number of terms in delay polynomial
C     GMMOD     R           Mean gain modulus
C
C     NKEYS     I           Number of keywords to read from IM table
C     KEYS      C(NKEYS)*8  List of keywords to read from IM table
C     KLOCS     I(NKEYS)    Offsets of values in KVALS
C     KVALS     I(2*NKEYS)  Keyword value buffer
C     KTYPE     I(NKEYS)    Keyword types
C
C     TYPE      I           Attribute type code
C     DIM       I(3)        Attribute array dimensions
C     NDUMMY    I           Dummy numeric value
C
      CHARACTER DATTYP*2
      LOGICAL   EXISTS
      INTEGER   CALROW, NTERM
      REAL      GMMOD
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 1)
      CHARACTER KEYS(NKEYS)*8
      INTEGER   KLOCS(NKEYS), KVALS(2*NKEYS), KTYPE
      SAVE      KEYS
C
      INTEGER   TYPE, DIM(3), NDUMMY
C
      INTEGER   MCROW, NSTOKE, STOKE1, NUMIF, NCHAN, NUMPOL, FFTSIZ,
     *          OVRSMP, ZEROPD
      CHARACTER OBSCOD*8, RDATE*8, TAPER*8
      DOUBLE PRECISION RFREQ
      REAL      CHANBW, REFPIX
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IMMCTAB.INC'
C
      DATA KEYS /'NPOLY   '/
C-----------------------------------------------------------------------
      IMTAB = '        '
      MCTAB = '        '
C
      CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, NDUMMY, DATTYP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       interferometry data
      IF (DATTYP(:1).EQ.'U') THEN
         CALTAB = 'CL table'
         CALL UV2TAB (UVDATA, CALTAB, 'CL', 1, IRET)
         IF (IRET.NE.0) GO TO 999
C
C        Override request to create calibration table if one already
C        exists otherwise create the new table and open IM and MC
C        tables if these will be used to initialize the CL table:
C
         CALL TABEXI (CALTAB, EXISTS, IRET)
         IF (IRET.EQ.2) IRET = 0
         IF (IRET.NE.0) GO TO 999
         IF (EXISTS) THEN
            MSGTXT = 'A CL table already exists: a new one will not '
     *               // 'be created'
            CALL MSGWRT (5)
            CALL TABDES (CALTAB, IRET)
            IF (IRET.NE.0) GO TO 999
            CALTAB = '    '
            IMTAB  = '    '
            MCTAB  = '    '
            WXTAB  = '    '
            CPARM(3) = -1.0
         ELSE
            MSGTXT = 'Creating CL table with version number 1'
            CALL MSGWRT (5)
C                                       WX table
            IF (WXTAB.NE.' ') THEN
               CALL UV2TAB (UVDATA, WXTAB, 'WX', 1, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL TABEXI (WXTAB, EXISTS, IRET)
               IF (IRET.EQ.2) IRET = 0
               IF (IRET.NE.0) GO TO 999
               IF (EXISTS) THEN
                  MSGTXT = 'Will use weather from WX table 1'
                  CALL MSGWRT (5)
                  MSGTXT = 'Sorting WX table 1 into time order'
                  CALL MSGWRT (5)
                  CALL TBLSRT (WXTAB, 'TIME', 'TIME', IRET)
                  IF (IRET.NE.0) GO TO 999
               ELSE
                  MSGTXT = 'WX table 1 does not exist'
                  CALL MSGWRT (5)
                  MSGTXT = 'Overriding request to use WX table' //
     *               ': doing seasonal model only'
                  CALL MSGWRT (5)
                  CALL TABDES (WXTAB, IRET)
                  IF (IRET.NE.0) GO TO 999
                  WXTAB = ' '
C                  BPARM(1) = -1.0
                  BPARM(10) = 0.0
                  END IF
               END IF
C                                       Open IM table
            IF (CPARM(4).GT.0.0) THEN
               IMTAB = 'IM table'
               CALL UV2TAB (UVDATA, IMTAB, 'IM', 1, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL TABEXI (IMTAB, EXISTS, IRET)
               IF (IRET.EQ.2) IRET = 0
               IF (IRET.NE.0) GO TO 999
               IF (EXISTS) THEN
                  MSGTXT = 'Will use geometric delays from IM table 1'
                  CALL MSGWRT (5)
                  MSGTXT = 'Sorting IM table 1 into time order'
                  CALL MSGWRT (5)
                  CALL TBLSRT (IMTAB, 'TIME', 'TIME', IRET)
                  IF (IRET.NE.0) GO TO 999
               ELSE
                  MSGTXT = 'IM table 1 does not exist'
                  CALL MSGWRT (5)
                  MSGTXT = 'Overriding request to use IM table'
                  CALL MSGWRT (5)
                  CALL TABDES (IMTAB, IRET)
                  IF (IRET.NE.0) GO TO 999
                  IMTAB = '    '
                  CPARM(4) = 0.0
                  END IF
            ELSE
               IMTAB = '    '
               END IF
            IF (CPARM(5).GT.0.0) THEN
               MCTAB = 'MC table'
               CALL UV2TAB (UVDATA, MCTAB, 'MC', 1, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL TABEXI (MCTAB, EXISTS, IRET)
               IF (IRET.EQ.2) IRET = 0
               IF (IRET.NE.0) GO TO 999
               IF (EXISTS) THEN
                  MSGTXT = 'Will use clocks and atmosphere from MC '
     *                     // 'table 1'
                  CALL MSGWRT (5)
                  MSGTXT = 'Sorting MC table 1 into time order'
                  CALL MSGWRT (5)
                  CALL TBLSRT (MCTAB, 'TIME', 'TIME', IRET)
                  IF (IRET.NE.0) GO TO 999
               ELSE
                  MSGTXT = 'MC table 1 does not exist'
                  CALL MSGWRT (5)
                  MSGTXT = 'Overriding request to use MC table'
                  CALL MSGWRT (5)
                  CALL TABDES (MCTAB, IRET)
                  IF (IRET.NE.0) GO TO 999
                  MCTAB = '    '
                  CPARM(5) = 0.0
                  END IF
            ELSE
               MCTAB = '    '
               END IF
C                                       Assume 1 polynomial term for VLA
            NTERM = 1
            GMMOD = 1.0
            IF (IMTAB.NE.' ') THEN
               NUMIMR = 0
               CALL OIMINI (IMTAB, 'READ', IRET)
               IF (IRET.NE.0) GO TO 999
               CALL TABKGT (IMTAB, KEYS, NKEYS, KLOCS, KVALS, KTYPE,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               IF (KLOCS(1).LT.0) THEN
                  MSGTXT = 'WARNING: No NPOLY keyword in IM table.'
     *                     // ' assuming NPOLY = 6'
                  CALL MSGWRT (6)
                  NTERM = 6
               ELSE
                  NTERM = KVALS(KLOCS(1))
                  END IF
               END IF
            CALL OCLINI (CALTAB, 'WRIT', CALROW, NSTA, NPOL, NIF, NTERM,
     *         GMMOD, IRET)
            IF (IRET.NE.0) GO TO 999
            IF (MCTAB.NE.'    ') THEN
               NUMMCR = 0
               CALL OMCINI (MCTAB, 'READ', MCROW, OBSCOD, RDATE, NSTOKE,
     *            STOKE1, NUMIF, NCHAN, RFREQ, CHANBW, REFPIX, NUMPOL,
     *            FFTSIZ, OVRSMP, ZEROPD, TAPER, MCDELT, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
C                                       Single-dish data
      ELSE
         CALTAB = 'CS table'
         CALL UV2TAB (UVDATA, CALTAB, 'CS', 1, IRET)
         IF (IRET.NE.0) GO TO 999
C
C        Override request to create calibration table if one already
C        exists otherwise create the new table:
C
         CALL TABEXI (CALTAB, EXISTS, IRET)
         IF (IRET.EQ.2) IRET = 0
         IF (IRET.NE.0) GO TO 999
         IF (EXISTS) THEN
            MSGTXT = 'A CS table already exists: a new one will not '
     *               // 'be created'
            CALL MSGWRT (5)
            CALL TABDES (CALTAB, IRET)
            IF (IRET.NE.0) GO TO 999
            CALTAB = '    '
            CPARM(3) = -1.0
         ELSE
            MSGTXT = 'Creating CS table with version number 1'
            CALL MSGWRT (5)
            CALL OCSINI (CALTAB, 'WRIT', CALROW, NSTA, NPOL, NIF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
C
  999 RETURN
      END
LOCAL INCLUDE 'SCANS.INC'
C
C Local include containing private information for the scan manager.
C The scan manager is responsible for applying the rules that
C determine where a scan begins and ends and when a calibration
C record is written out.  The scan manager should be initialized
C by calling INISCN.  After initializing the scan manager, CHKSCN
C should be called for each visibility record in the input data
C file.  After reaching the end of the data, FINSCN should be
C called to finalize the scan manager and to complete any "open"
C scans.
C
C Implementation variables for the scan manager.  There may be one
C current subscan for each subarray.  Variables maintaining the
C information on current scans are indexed by subarray number.
C
C MAXSUB    I                  Maximum number of subarrays (parameter)
C MAXBRK    I                  Maximum number of breaks per subarray
C                               (parameter)
C
C NUMANT    I(MAXSUB)          Number of antennae/beams encounted in
C                               the current scan for each subarray.  A
C                               value of zero indicates that there is
C                               no current scan for the subarray in
C                               question.
C ANTS      I(MAXANT, MAXSUB)  List of antennae/beams encountered for
C                               each current scan.
C VSTART    I(MAXSUB)          Index number of first visibility of
C                               current scan
C VEND      I(MAXSUB)          Index number of last visibility of
C                               current scan
C STIME     R(MAXSUB)          Start time of current scan in days
C ETIME     R(MAXSUB)          Last time in current scan in days
C CSTIME    R(MAXSUB)          Last time in current scan corresponding
C                               to a CS table record in days
C CSANTS    I(MAXSUB)          Number of beams encountered in the
C                               current scan up to the time of the
C                               last CS table record
C OBSRA     R(MAXANT, MAXSUB)  Pointing RA for each beam
C OBSDEC    R(MAXANT, MAXSUB)  Pointing declination for each beam
C SRC       I(MAXSUB)          Current source ID number
C SCAN      I(MAXSUB)          Current scan ID number
C FQID      I(MAXSUB)          Current FQID number
C
C MAXGAP    R                  Maximum gap allowed in a scan in days
C MAXLEN    R                  Maximum scan length in days
C CALINC    R                  Increment between calibration records
C                               in days
C NUMPOL    I                  Number of polarization in calibration
C                               files
C NUMIF     I                  Number of IFs in data
C NUMSTA    I                  Number of antennae/beams in largest
C                               subarray
C CALTBL    C*(32)             Name of calibration table object:
C                               blank if not used
C NXTBL     C*(32)             Name of index table object
C IMTBL     C*(32)             Name of interferometer table object:
C                               blank if not used
C MCTBL     C*(32)             Name of model components table object:
C                               blank if not used
C VIS       I                  Current visibility number
C CALROW    I                  Next calibration record to write.
C NXROW     I                  Next index record to write
C WRITCL    L                  Write CL table?
C WRITCS    L                  Write CS table?
C
C BREAKS    R(MAXBRK, MAXSUB)  List of forced scan breaks for each
C                              subarray, sorted in order of
C                              descending time and including a time
C                              far beyond the end of the data.
C NXTBRK    I(MAXSUB)          Pointer to the smallest item in
C                              BREAKS(:, MAXSUB) greater than the
C                              last time read for each subarray
C
C If 1 <= j <= MAXSUB and 1 <= i <= NUMANT(j) then
C    0 <= NUMANT(i) <= NUMSTA <= MAXANT.
C    ANTS(i, j) > 0
C    VEND(j) >= VSTART(j) > 0
C    ETIME(j) >= STIME(j)
C    ETIME(j) >= CSTIME(j) >= STIME(j) when writing a CS table
C    NUMANT(j) >= CSANTS(j) >= 0 when writing a CS table
C    1 <= NXTBRK(j) <= MAXBRK
C
C   Special VLBI mode: all subarray are the same set of antennas
C   SUBANT   I(MAXANT)   Current subarray by antenna
C   STIANT   R(MAXANT)   Start time of this antenna in this subarray
C   ETIANT   R(MAXANT)   Most recent time this antenna this subarray
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSUB, MAXBRK
      PARAMETER (MAXSUB = 512)
      PARAMETER (MAXBRK = 5000)
C
      INTEGER   NUMANT(MAXSUB), ANTS(MAXANT, MAXSUB), VSTART(MAXSUB),
     *   VEND(MAXSUB), CSANTS(MAXSUB), SRC(MAXSUB), SCAN(MAXSUB),
     *   FQID(MAXSUB), NXTBRK(MAXSUB), NUMPOL, NUMIF, NUMSTA, VIS,
     *   CALROW, NXROW, SUBANT(MAXANT), NGAPS
      REAL      STIME(MAXSUB), ETIME(MAXSUB), CSTIME(MAXSUB),
     *   OBSRA(MAXANT,MAXSUB), OBSDEC(MAXANT,MAXSUB),
     *   BREAKS(MAXBRK,MAXSUB), MAXGAP, MAXLEN, CALINC, STIANT(MAXANT),
     *   ETIANT(MAXANT), SGAPS
      LOGICAL   WRITCL, WRITCS, VLBMOD
      CHARACTER CALTBL*32, NXTBL*32, IMTBL*32, MCTBL*32, WXTBL*32
C
      COMMON /SCNPRV/ NUMANT, ANTS, VSTART, VEND, STIME, ETIME, CSTIME,
     *   CSANTS, OBSRA, OBSDEC, SRC, SCAN, FQID, MAXGAP, MAXLEN, CALINC,
     *   NUMPOL, NUMIF, NUMSTA, VIS, CALROW, NXROW, WRITCL, WRITCS,
     *   BREAKS, NXTBRK, SUBANT, STIANT, ETIANT, VLBMOD, NGAPS, SGAPS
      COMMON /CSCNPR/ CALTBL, NXTBL, IMTBL, MCTBL, WXTBL
      SAVE /SCNPRV/, /CSCNPR/
C
LOCAL END
      SUBROUTINE INISCN (UVDATA, INFILE, PRTLEV, INDEX, CALTAB, IMTAB,
     *   MCTAB, WXTAB, GAP, LEN, INC, NPOL, NIF, NSTA, MODVLB, IRET)
C-----------------------------------------------------------------------
C   Initialize the scan manager module.
C   Inputs:
C      UVDATA   C*(*)   Name of UVDATA object for parent file
C      INFILE   C*(*)   Name of auxilliary input file
C      PRTLEV   I       Level of printout required
C      INDEX    C*(*)   Name of TABLE object for index table
C      CALTAB   C*(*)   Name of TABLE object for CL or CS table or blank
C                       if no calibration table is being written
C      IMTAB    C*(*)   Name of TABLE object for IM table or blank if no
C                       IM table is being used
C      MCTAB    C*(*)   Name of TABLE object for MC table or blank if no
C                       MC table is being used
C      GAP      R       Maximum gap within a scan in days
C      LEN      R       Maximum length of scan in days
C      INC      R       Increment between calibration records if writing
C                       a calibration table
C      NPOL     I       Number of polarizations in calibration table
C      NIF      I       Number of IFs in data
C      NSTA     I       Largest number of antennae/beams in a subarray
C      VLBMOD   L       Do special CL handling for VLB subarraying
C   Output
C      IRET     I       Return status: 0 indicates successful
C                       initialization anything else indicates failure
C   Preconditions:
C      All tables should be open: INDEX and CALTAB (if used) for
C          writing; IMTAB and MCTAB (if used) for reading.
C      GAP > 0.0
C      LEN > 0.0
C      If CALTAB is not blank then
C         INC > 0.0, 1 <= NPOL <= 2, 1 <= NIF <= MAXIF,
C         1 <= NSTA
C      where MAXIF is defined in INCS:PUVD.INC
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), INFILE*(*), INDEX*(*), CALTAB*(*),
     *   IMTAB*(*), MCTAB*(*), WXTAB*(*)
      REAL      GAP, LEN, INC
      INTEGER   PRTLEV, NPOL, NIF, NSTA, IRET
      LOGICAL   MODVLB
C                                       Local variables
C     TBTYPE    C*2       Type of calibration table
C
C     TYPE      I         Attribute type code
C     DIM       I(3)      Attribute dimensions
C     NDUMMY    I         Dummy numeric value
C
      CHARACTER TBTYPE*2
C
      INTEGER   TYPE, DIM(3), NDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SCANS.INC'
C-----------------------------------------------------------------------
      NXTBL  = INDEX
      CALTBL = CALTAB
      IMTBL  = IMTAB
      MCTBL  = MCTAB
      WXTBL =  WXTAB
      MAXGAP = GAP
      MAXLEN = LEN
      CALINC = INC
      NUMPOL = NPOL
      NUMIF  = NIF
      NUMSTA = NSTA
      VLBMOD = MODVLB
      NGAPS = 0
      SGAPS = 0
C                                       Initialize data structures for
C                                       forced scan breaks:
      CALL INIBRK (UVDATA, INFILE, PRTLEV, IRET)
      IF (IRET.NE.0) GO TO 999
C
      WRITCL = .FALSE.
      WRITCS = .FALSE.
      IF (CALTBL.NE.' ') THEN
         CALL TABGET (CALTAB, 'TBLTYPE', TYPE, DIM, NDUMMY, TBTYPE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (TBTYPE.EQ.'CL') THEN
            WRITCL = .TRUE.
         ELSE IF (TBTYPE.EQ.'CS') THEN
            WRITCS = .TRUE.
         ELSE
            MSGTXT = 'INISCN: INVALID CAL TABLE TYPE (' // TBTYPE
     *         // ')'
            CALL MSGWRT (10)
            MSGTXT = 'INTERNAL ERROR'
            CALL MSGWRT (10)
            IRET = 1
            GO TO 999
            END IF
         END IF
C
      IF (.NOT.WRITCL) VLBMOD = .FALSE.
      CALL FILL (MAXSUB, 0, NUMANT)
      CALL FILL (MAXSUB, 0, CSANTS)
      CALL FILL (MAXANT, -1, SUBANT)
      CALL RFILL (MAXANT, -999.0, STIANT)
      CALL RFILL (MAXANT, -999.0, ETIANT)
C
      VIS    = 0
      CALROW = 1
      NXROW  = 1
C
  999 RETURN
      END
      SUBROUTINE CHKSCN (TIME, A1, A2, CURSUB, CURSRC, CURSCN, CURFQI,
     *   CURRA, CURDEC, IRET)
C-----------------------------------------------------------------------
C   Check whether a new scan should be started for the current
C   visibility record and generate calibration records as required.
C   Detects "scan" breaks when VLB antennas switch subarray in the
C   special VLBMOD.
C   Inputs:
C      TIME     R   Visibility time stamp
C      A1       I   Lower number antenna
C      A2       I   Higher number antenna
C      CURSUB   I   sunarray
C      CURSRC   I   Source number
C      CURSCN   I   Scan number (for single dish)
C      CURFQI   I   Current frequency group
C      CURRA    R   Current apparent RA (single-dish)
C      CURDEC   R   Current apparent declination (single-dish)
C   Output:
C      IRET     I   Return status 0 indicates visibility checked
C                   anything else indicates an unrecoverable error
C-----------------------------------------------------------------------
      REAL      TIME, CURRA, CURDEC
      INTEGER   A1, A2, CURSUB, CURSRC, CURSCN, CURFQI, IRET
C                                       Local variables:
C     GAP       R       Interval from last time in scan in days
C     LENGTH    R       Interval from first time in scan in days
C     CTIME     R       Center time of scan in days
C     DTIME     R       Duration of scan in days
C
      REAL      GAP, LENGTH, CTIME, DTIME, TEPS
C
      INCLUDE 'SCANS.INC'
      INCLUDE 'INDXR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C        Note that interferometry calibration records are dumped in a
C        block for each scan so that there are entries covering the
C        entire scan for every antenna that participates in the scan.
C        This is not possible for single-dish calibration where the
C        records contain the RA and declination which vary over the scan
C        are are not known where there is no data.  Single-dish
C        calibration records are dumped out for all beams found in a
C        scan at each time that new beams are discovered in the data and
C        at intervals not exceeding CALINC with a terminal record at the
C        end of a scan.
C-----------------------------------------------------------------------
      VIS = VIS + 1
C                                       0.2 sec
      TEPS = 2.3E-6
C                                       VLB mode
      IF (VLBMOD) THEN
         IF ((SUBANT(A1).GT.0) .AND. (SUBANT(A1).NE.CURSUB)) THEN
            CALL DUMPCL (SUBANT(A1), A1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         IF ((SUBANT(A2).GT.0) .AND. (SUBANT(A2).NE.CURSUB)) THEN
            CALL DUMPCL (SUBANT(A2), A2, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         IF (SUBANT(A1).LE.0) THEN
            SUBANT(A1) = CURSUB
            STIANT(A1) = TIME
            ETIANT(A1) = -1000.
            END IF
         IF (SUBANT(A2).LE.0) THEN
            SUBANT(A2) = CURSUB
            STIANT(A2) = TIME
            ETIANT(A2) = -1000.
            END IF
         END IF
C                                       Start of scan
      IF (NUMANT(CURSUB).EQ.0) THEN
         VSTART(CURSUB) = VIS
         STIME(CURSUB)  = TIME
         SRC(CURSUB)    = CURSRC
         SCAN(CURSUB)   = CURSCN
         FQID(CURSUB)   = CURFQI
         OBSRA(A1, CURSUB)  = CURRA
         OBSRA(A2, CURSUB)  = CURRA
         OBSDEC(A1, CURSUB) = CURDEC
         OBSDEC(A2, CURSUB) = CURDEC
C                                       Time has changed
      ELSE IF (TIME.NE.ETIME(CURSUB)) THEN
C        It is possible for times to be out of order even if the
C        data set is marked as time-sorted.  If this happens INDXR
C        cannot do anything meaningful so check for it.
         IF (TIME.LT.ETIME(CURSUB)) THEN
            MSGTXT = 'QUITTING: FILE CONTAINS DATA WHICH IS OUT OF '
     *         // 'TIME ORDER'
            CALL MSGWRT (9)
            MSGTXT = 'UV FILE HEADER SORT ORDER IS INCORRECT'
            CALL MSGWRT (9)
            MSGTXT = 'Use UVSRT or MSORT to sort the file into '
     *         // ' time order'
            CALL MSGWRT (9)
            MSGTXT = 'and try again.'
            CALL MSGWRT (9)
            IRET = 1
            GO TO 999
            END IF
C                                       TIME > ETIME(CURSUB)
         GAP    = TIME - ETIME(CURSUB)
         LENGTH = TIME - STIME(CURSUB)
C
C        Dump new CS table records if the number of beams
C        increased at the previous time in the scan or if
C        at least CALINC days have elapsed since the last
C        CS record:
C
         IF (WRITCS) THEN
            IF ((NUMANT(CURSUB).GT.CSANTS(CURSUB))
     *           .OR. ((TIME - CSTIME(CURSUB)).GE.CALINC)) THEN
               CALL DUMPCS (TIME, CURSUB, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
C                                       try to find proper maxgap
         IF ((ADAPT) .AND. (NGAPS.GE.0) .AND. (GAP.GT.tEPS) .AND.
     *      (GAP.LE.MAXGAP) .AND. (CURSRC.EQ.SRC(CURSUB)) .AND.
     *      (CURFQI.EQ.FQID(CURSUB))) THEN
            IF (NGAPS.EQ.0) THEN
               NGAPS = 1
               SGAPS = GAP
            ELSE IF ((NGAPS.GT.1) .AND. (GAP.GT.10.0*SGAPS/NGAPS)) THEN
               MAXGAP = 10.0*SGAPS/NGAPS
               NGAPS = -1
            ELSE
               NGAPS = NGAPS + 1
               SGAPS = SGAPS + GAP
               END IF
            END IF
C
C        Close out the current scan and start a new one if the
C        maximum gap or scan length is exceeded or if the source,
C        scan or frequency group ID has changed:
C
         IF ((LENGTH.GT.MAXLEN) .OR. (GAP.GT.MAXGAP)
     *       .OR. (CURSRC.NE.SRC(CURSUB))
     *       .OR. (CURSCN.NE.SCAN(CURSUB))
     *       .OR. (CURFQI.NE.FQID(CURSUB))
     *       .OR. (TIME.GE.BREAKS(NXTBRK(CURSUB), CURSUB))) THEN
            CTIME = (ETIME(CURSUB) + STIME(CURSUB)) / 2.0
            DTIME = ETIME(CURSUB) - STIME(CURSUB)
C                                       NB: previous versions used TIME
C                                       in place of ETIME(CURSUB) ---
C                                       revert to this if there are
C                                       problems with this scheme
            CALL OTABNX (NXTBL, 'WRIT', NXROW, CTIME, DTIME,
     *         SRC(CURSUB), CURSUB, VSTART(CURSUB), VEND(CURSUB),
     *         FQID(CURSUB), IRET)
            IF (IRET.NE.0) GO TO 999
            IF (WRITCS) THEN
               CALL DUMPCS (ETIME(CURSUB), CURSUB, IRET)
               IF (IRET.NE.0) GO TO 999
            ELSE IF (WRITCL) THEN
               CALL DUMPCL (CURSUB, 0, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Set up new scan:
            IF (VLBMOD) THEN
               IF (SUBANT(A1).LE.0) THEN
                  SUBANT(A1) = CURSUB
                  STIANT(A1) = TIME
                  END IF
               IF (SUBANT(A2).LE.0) THEN
                  SUBANT(A2) = CURSUB
                  STIANT(A2) = TIME
                  END IF
               END IF
            NUMANT(CURSUB) = 0
            CSANTS(CURSUB) = 0
            VSTART(CURSUB) = VIS
            STIME(CURSUB)  = TIME
            SRC(CURSUB)    = CURSRC
            SCAN(CURSUB)   = CURSCN
            FQID(CURSUB)   = CURFQI
            OBSRA(A1,CURSUB)  = CURRA
            OBSRA(A2,CURSUB)  = CURRA
            OBSDEC(A1,CURSUB) = CURDEC
            OBSDEC(A2,CURSUB) = CURDEC
 10         IF (TIME.GT.BREAKS(NXTBRK(CURSUB), CURSUB)) THEN
               NXTBRK(CURSUB) = NXTBRK(CURSUB) - 1
               GO TO 10
               END IF
            END IF
         END IF
C                                       Update scan information:
      VEND(CURSUB)      = VIS
      ETIME(CURSUB)     = TIME
      OBSRA(A1,CURSUB)  = CURRA
      OBSRA(A2,CURSUB)  = CURRA
      OBSDEC(A1,CURSUB) = CURDEC
      OBSDEC(A2,CURSUB) = CURDEC
      CALL ADDANT (A1, CURSUB, IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT, 9010) MAXANT, CURSUB
         CALL MSGWRT (9)
         GO TO 999
         END IF
      CALL ADDANT (A2, CURSUB, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 9010) MAXANT, CURSUB
         CALL MSGWRT (9)
         IRET = 1
         GO TO 999
         END IF
      ETIANT(A1) = TIME
      ETIANT(A2) = TIME
C
  999 RETURN
C-----------------------------------------------------------------------
 9010 FORMAT ('CAN NOT HANDLE MORE THAN ', I4, ' ANTENNAS IN A ',
     *        'SUBARRAY (', I2, ')')
      END
      SUBROUTINE FINSCN (IRET)
C-----------------------------------------------------------------------
C   Finalize the scan manager, writing out any information for scans
C   that have not yet been closed out.
C
C   Output:
C      IRET     I       Return status:
C                        0 indicates scan manager finalized
C                        anything else indicates unrecoverable error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
C     Local variables
C
C     CTIME     R       Center time of scan in days
C     DTIME     R       Duration of scan in days
C     S         I       Subarray number
C
      REAL      CTIME, DTIME
      INTEGER   S
C
      INCLUDE 'SCANS.INC'
C-----------------------------------------------------------------------
      DO 10 S = 1, MAXSUB
         IF (NUMANT(S).GT.0) THEN
            CTIME = (ETIME(S) + STIME(S)) / 2.0
            DTIME = ETIME(S) - STIME(S)
            CALL OTABNX (NXTBL, 'WRIT', NXROW, CTIME, DTIME, SRC(S), S,
     *         VSTART(S), VEND(S), FQID(S), IRET)
            IF (IRET.NE.0) GO TO 999
            IF (WRITCS) THEN
               CALL DUMPCS (ETIME(S), S, IRET)
               IF (IRET.NE.0) GO TO 999
            ELSE IF (WRITCL) THEN
               CALL DUMPCL (S, 0, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
 10      CONTINUE
C
  999 RETURN
      END
      SUBROUTINE ADDANT (A, SUBARR, IRET)
C-----------------------------------------------------------------------
C   Add antenna A to the current scan for SUBARR
C
C   Inputs:
C      A        I       Antenna/beam number
C      SUBARR   I       Subarray number
C
C   Output:
C      IRET     I       Status: 0 - antenna added to SUBARR
C                               1 - no room left to add antenna
C
C   Preconditions:
C      0 <= NUMANT(SUBARR) <= MAXANT
C      1 <= SUBARR <= MAXSUB
C-----------------------------------------------------------------------
      INTEGER   A, SUBARR, IRET
C
      INTEGER   I
      LOGICAL   FOUND
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SCANS.INC'
C-----------------------------------------------------------------------
      IRET = 0
      FOUND = .FALSE.
      I = 0
   10 IF ((I.NE.NUMANT(SUBARR)) .AND. (.NOT. FOUND)) THEN
         I = I + 1
         IF (ANTS(I, SUBARR).EQ.A) THEN
            FOUND = .TRUE.
         END IF
         GO TO 10
      END IF
C
      IF (.NOT. FOUND) THEN
         IF (NUMANT(SUBARR) .LT. MAXANT) THEN
            NUMANT(SUBARR) = NUMANT(SUBARR) + 1
            ANTS(NUMANT(SUBARR), SUBARR) = A
         ELSE
            IRET = 1
         END IF
      END IF
C
      END
      SUBROUTINE DUMPCS (TIME, SUBARR, IRET)
C-----------------------------------------------------------------------
C   Dump CS records for SUBARR at TIME.
C
C   Inputs:
C      TIME    R       Time stamp (days)
C      SUBARR  I       Subarray number
C
C   Output:
C      IRET    I       Return code:
C                       0 indicates CS records written
C                       anything else indicates an unrecoverable error
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   SUBARR, IRET
C
C     Local variables
C
C     A         I             Beam number
C     CSFACT    R(2, MAXIF)   Calibration factors
C     CSOFF     R(2, MAXIF)   Calibration offsets
C     CSRAOF    R(2, MAXIF)   RA offsets
C     CSDCOF    R(2, MAXIF)   Declination offsets
C
      INCLUDE 'SCANS.INC'
      INTEGER   A
      REAL      CSFACT(2, MAXIF), CSOFF(2, MAXIF), CSRAOF(2, MAXIF),
     *          CSDCOF(2, MAXIF)
C-----------------------------------------------------------------------
      CALL RFILL (2 * MAXIF, 1.0, CSFACT)
      CALL RFILL (2 * MAXIF, 0.0, CSOFF)
      CALL RFILL (2 * MAXIF, 0.0, CSRAOF)
      CALL RFILL (2 * MAXIF, 0.0, CSDCOF)
C
      DO 10 A = 1, NUMANT(SUBARR)
         CALL OTABCS (CALTBL, 'WRIT', CALROW, NUMPOL, TIME,
     *      OBSRA(ANTS(A, SUBARR), SUBARR), OBSDEC(ANTS(A, SUBARR),
     *      SUBARR), ANTS(A, SUBARR), SUBARR, CSFACT, CSOFF, CSRAOF,
     *      CSDCOF, IRET)
         IF (IRET.NE.0) GO TO 999
 10      CONTINUE
C
      CSANTS(SUBARR) = NUMANT(SUBARR)
      CSTIME(SUBARR) = TIME
C
  999 RETURN
      END
      SUBROUTINE DUMPCL (SUBARR, DMPANT, IRET)
C-----------------------------------------------------------------------
C   Write CL table records covering the current scan for SUBARR
C   Input:
C      SUBARR   I   Subarray number
C      DMPANT   I   Restrict to antenna number; 0 => all
C   Output:
C      IRET     I   Return code: 0 indicates CL table records written
C                   anything else indicates an unrecoverable error
C-----------------------------------------------------------------------
      INTEGER   SUBARR, DMPANT, IRET
C                                       Local variables:
C     A        I           Antenna number
C     TIME     R           Time stamp (days)
C     IFR      R           Ionospheric Faraday rotation
C     GEODLY   D(16)       Geometric delay coefficients
C     ATMOS    R           Atmospheric delay
C     DATMOS   R           Derivative of atmospheric delay
C     DOPOFF   R(MAXIF)    Doppler offsets
C     MBDELY   R(2)        Multiband delays
C     CLOCK    R(2)        Clock offsets
C     DCLOCK   R(2)        Derivatives of clock offsets
C     DISP     R(2)        Dispersive delays
C     DDISP    R(2)        Derivatives of dispersive delays
C     CREAL    R(2, MAXIF) Real gains
C     CIMAG    R(2, MAXIF) Imaginary gains
C     DELAY    R(2, MAXIF) Delays
C     RATE     R(2, MAXIF) Rates
C     WEIGHT   R(2, MAXIF) Solution weights
C     REFA     I(2, MAXIF) Reference antennae
C
      INCLUDE 'SCANS.INC'
      INTEGER   A, REFA(2,MAXIF), A1, A2, AA, ISUB
      REAL      IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      DOUBLE PRECISION GEODLY(16), TIME, TB, TE, DELTAT
      LOGICAL   SAME
      INCLUDE 'VLAGN.INC'
      INCLUDE 'IMMCTAB.INC'
C
      DATA GEODLY / 16 * 0.0 /
      DATA IFR, ATMOS, DATMOS / 3 * 0.0 /
      DATA MBDELY / 2 * 0.0 /
      DATA CLOCK, DCLOCK / 2 * 0.0, 2 * 0.0 /
      DATA DISP, DDISP / 2 * 0.0, 2 * 0.0 /
      DATA DOPOFF / MAXIF * 0.0 /
      DATA CREAL  / MAXIF * 1.0, MAXIF * 1.0 /
      DATA CIMAG  / MAXIF * 0.0, MAXIF * 0.0 /
      DATA DELAY  / MAXIF * 0.0, MAXIF * 0.0 /
      DATA RATE   / MAXIF * 0.0, MAXIF * 0.0 /
      DATA WEIGHT / MAXIF * 1.0, MAXIF * 1.0 /
      DATA REFA   / MAXIF * 0, MAXIF * 0/
C-----------------------------------------------------------------------
      DELTAT = 0.01D0 / (24.0D0 * 3600.0D0)
      ISUB = SUBARR
      IF (VLBMOD) ISUB = 0
      IF (DMPANT.LE.0) THEN
         A1 = 1
         A2 = NUMANT(SUBARR)
      ELSE
         A1 = DMPANT
         A2 = DMPANT
         END IF
C                                       loop over antenna
      DO 20 A = A1,A2
C                                       antenna number
         IF (DMPANT.LE.0) THEN
            AA = ANTS(A,SUBARR)
         ELSE
            AA = A
            END IF
C                                       time range
         IF (VLBMOD) THEN
            TB = STIANT(AA)
            TE = ETIANT(AA)
C                                       only in this subarray
            IF (SUBANT(AA).NE.SUBARR) GO TO 20
         ELSE
            TB = STIME(SUBARR)
            TE = ETIME(SUBARR)
            END IF
         SAME = TE-TB.LT.DELTAT
         IF (.NOT.SAME) THEN
            TB = TB - DELTAT
            TE = TE + DELTAT
            END IF
         IF (TB.GT.TE) GO TO 20
C                                       loop over time
         TIME = TB
 10      IF (TIME.LE.TE) THEN
            IF (IMTBL.NE.' ') THEN
               CALL OGTDEL (IMTBL, TIME, SRC(SUBARR), AA, ISUB,
     *            FQID(SUBARR), GEODLY, DISP, DDISP, NUMIMR, IRET)
               IF (IRET.GT.0) GO TO 999
C                                       Force dispersive delays to zero
C                                       They are applied by the cal
C                                       routines so we cannot keep
C                                       whatever was already applied
               DISP(1) = 0.0
               DISP(2) = 0.0
               DDISP(1) = 0.0
               DDISP(2) = 0.0
               END IF
            IF (MCTBL.NE.' ') THEN
               CALL OGETMC (MCTBL, MCDELT, TIME, SRC(SUBARR), AA, ISUB,
     *            FQID(SUBARR), CLOCK, DCLOCK, ATMOS, DATMOS, NUMMCR,
     *            IRET)
               IF (IRET.GT.0) GO TO 999
               END IF
            IF ((OPACIT.NE.0) .OR. (GNTYP.NE.0)) THEN
               CALL INDWXR (CALTBL, WXTBL, TIME, SRC(SUBARR), AA,
     *            SUBARR, FQID(SUBARR), NUMPOL, NUMIF, CREAL, IRET)
               IF (IRET.GT.0) GO TO 999
               END IF
            CALL OTABCL (CALTBL, 'WRIT', CALROW, NUMPOL, NUMIF, TIME,
     *         CALINC, SRC(SUBARR), AA, SUBARR, FQID(SUBARR), IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((TIME.LT.TE) .AND. (.NOT.SAME)) THEN
               TIME = MIN (TIME+CALINC, TE)
               IF (TE-TIME.LT.0.05*CALINC) TIME = TE
               GO TO 10
               END IF
            END IF
         SUBANT(AA) = 0
 20      CONTINUE
C
  999 RETURN
      END
      SUBROUTINE INIBRK (UVDATA, INFILE, PRTLEV, IRET)
C-----------------------------------------------------------------------
C   Initialize the data structures for forced scan breaks.
C
C   Inputs:
C      UVDATA   C*(*)      Name of UVDATA object used to access parent
C                           data file
C      INFILE   C*(*)      Name of text file containing break time
C                           specifications or blank if there are no
C                           forced breaks
C      PRTLEV   I          Amount of printout required while reading
C                           INFILE (<= 0 gives no printout, >= 1 echoes
C                           file
C
C   Output:
C      IRET     I          Status indicator: 0 indicates succesful
C                           initialization, anything else a fatal error
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), INFILE*(*)
      INTEGER   PRTLEV, IRET
C
C     Local variables
C
C     DATE     Temporary date and time broken into integer components
C     RDATE    UV reference date as a character string
C     OBSDAY   Julian day number at reference epoch
C     REFDAY   Julian day number at Jan 0
C     ZERO     Day of year at reference epoch
C
C     LUN      AIPS LUN for text file (parameter)
C     FIND     AIPS file index for text file
C
C     KEYS     List of keywords for input file
C     VALUES   Numeric values returned by KEYIN
C     VALCHR   Character values returned by KEYIN
C     N        Number of KEYIN values
C     ENDMRK   KEYIN end marker (parameter)
C     MODE     KEYIN mode
C
C     EOF      Has end-of-file been encountered?
C
C     SUB      Subarray number
C     LSTBRK   "High-water marks" for BREAKS array
C
C     TYPE     Attribute type
C     DIM      Attribute dimensions
C     NDUMMY   Dummy numeric value
C     I        Loop counter
C
      INTEGER   DATE(6), ZERO
      CHARACTER RDATE*8
      DOUBLE PRECISION OBSDAY, REFDAY
C
      INTEGER   LUN, FIND
      PARAMETER (LUN = 10)
C
      INCLUDE 'SCANS.INC'
C
      CHARACTER KEYS(1)*8, VALCHR(2 * MAXBRK)*8, ENDMRK*8
      DOUBLE PRECISION VALUES(2 * MAXBRK)
      INTEGER   N, MODE
      PARAMETER (ENDMRK = '/       ')
C
      LOGICAL   EOF
C
      INTEGER   SUB
C
      INTEGER   TYPE, DIM(3), NDUMMY, I
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA KEYS / 'SUBARRAY' /
C-----------------------------------------------------------------------
      IRET = 0
C
C     Ensure BREAKS and NXTBRK arrays are valid:
C
      DO 10 I = 1, MAXSUB
         NXTBRK(I) = 1
         BREAKS(1, I) = 999.999
   10 CONTINUE
C
C     Read the text file if there is one:
C
      IF (INFILE.NE.'    ') THEN
C
C        Establish the day number for the observations:
C
         CALL UVDGET (UVDATA, 'DATE-OBS', TYPE, DIM, NDUMMY, RDATE,
     *                IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'INIBRK: CAN NOT READ REFERENCE DATE FROM UV FILE'
            CALL MSGWRT (10)
            GO TO 999
            END IF
         CALL DATEST (RDATE, DATE)
         DO 20 I = 4, 6
            DATE(I) = 0
   20    CONTINUE
         CALL DAT2JD (DATE, OBSDAY)
         DATE(2) = 1
         DATE(3) = 0
         CALL DAT2JD (DATE, REFDAY)
         ZERO = ANINT (OBSDAY - REFDAY)
C
C        Open the text file:
C
         CALL ZTXOPN ('READ', LUN, FIND, INFILE, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'INIBRK: CAN NOT OPEN ' // INFILE
            CALL MSGWRT (10)
            MSGTXT = 'Check value of INFILE.'
            CALL MSGWRT (10)
            GO TO 999
            END IF
         EOF = .FALSE.
C
C        Loop over blocks in the file (simulated while loop):
C
   30    IF (.NOT. EOF) THEN
C
C           Read header:
C
            IF (PRTLEV.GT.0) THEN
               MODE = 1
            ELSE
               MODE = 0
               END IF
            N = 1
            VALUES(1) = -1.0
            CALL KEYIN (KEYS, VALUES, VALCHR, N, ENDMRK, MODE, LUN,
     *                  FIND, IRET)
            IF (IRET.EQ.1) THEN
               EOF = .TRUE.
               IRET = 0
            ELSE IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 9030) IRET
               CALL MSGWRT (10)
               GO TO 999
               END IF
            IF (.NOT. EOF) THEN
               SUB = ANINT (VALUES(1))
               IF ((SUB.LT.1) .OR. (SUB.GT.MAXSUB)) THEN
                  MSGTXT = 'INVALID TIME SPECIFICATION BLOCK OR BAD '
     *                     // 'SUBARRAY NUMBER'
                  CALL MSGWRT (10)
                  WRITE (MSGTXT, 9031) MAXSUB
                  CALL MSGWRT (10)
                  IRET = 1
                  GO TO 999
                  END IF
C                                       MODE is 0 or 1
               MODE = MODE + 3
               N = 2 * MAXBRK
               CALL KEYIN (KEYS, VALUES, VALCHR, N, ENDMRK, MODE, LUN,
     *                     FIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT, 9030) IRET
                  CALL MSGWRT (10)
                  MSGTXT = 'File may be incomplete or corrupted'
                  CALL MSGWRT (10)
                  GO TO 999
                  END IF
C
C              Check that N is even:
C
               IF (MOD (N, 2).NE.0) THEN
                  MSGTXT = 'MISSING DAY NUMBER OR TIME IN INPUT FILE'
                  CALL MSGWRT (10)
                  IRET = 2
                  GO TO 999
                  END IF
C
C              Check for overflow:
C
               IF ((NXTBRK(SUB) + N/2).GT.MAXBRK) THEN
                  WRITE (MSGTXT, 9032) SUB
                  CALL MSGWRT (10)
                  WRITE (MSGTXT, 9033) MAXBRK
                  CALL MSGWRT (10)
                  IRET = 3
                  GO TO 999
                  END IF
C
C              Add new times:
C
               DO 40 I = 1, N/2
                  NXTBRK(SUB) = NXTBRK(SUB) + 1
                  BREAKS(NXTBRK(SUB), SUB) = VALUES(2 * I - 1) - ZERO
     *                                       + VALUES(2 * I) / 24.0D0
   40          CONTINUE
               END IF
            GO TO 30
            END IF
C
C        Close the text file:
C
         CALL ZTXCLS (LUN, FIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 9040) IRET
            CALL MSGWRT (10)
            GO TO 999
            END IF
C
         END IF
C
C     Sort the time values for each subarray:
C
      DO 50 I = 1,MAXSUB
         CALL RSORT (NXTBRK(I), BREAKS(1, I))
 50      CONTINUE
C
  999 RETURN
C-----------------------------------------------------------------------
 9030 FORMAT ('INIBRK: ERROR ', I3, ' READING TEXT FILE')
 9031 FORMAT ('Valid subarray numbers range from 1 to ', I3)
 9032 FORMAT ('TOO MANY TIMES SPECIFIED FOR SUBARRAY ', I3)
 9033 FORMAT ('LIMIT IS ', I5)
 9040 FORMAT ('INIBRK: ERROR ', I3, ' CLOSING TEXT FILE')
      END
      SUBROUTINE INDWXR (CALTBL, WXTBL, TIME, SRC, ANT, SUB, FQID,
     *   NUMPOL, NUMIF, CREAL, IRET)
C-----------------------------------------------------------------------
C   Determines the VLA gain correction for opacity and antenna gains
C   Inputs:
C      CALTBL   C*(*)   Calibration table (assoc table)
C      TIME     D       Time
C      SRC      I       Source number
C      ANT      I       Antenna number
C      SUB      I       Subarray number
C      FQID     I       Frequency ID
C      NUMPOL   I       Number polarizations in CREAL
C      NUMIF    I       Number IFs in CREAL
C   In/out:
C      WXTBL    C*(*)   Name of WX table object
C   Outputs:
C      CREAL    R(*)    Real part of gain table
C      IRET     I       Error code
C-----------------------------------------------------------------------
      CHARACTER CALTBL*(*), WXTBL*(*)
      DOUBLE PRECISION TIME
      INTEGER   SRC, ANT, SUB, FQID, NUMPOL, NUMIF, IRET
      REAL      CREAL(2,*)
C
      INCLUDE 'VLAGN.INC'
      INCLUDE 'INDXR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   IBAND, WANT, IPOL, IIF, I, NFG
      REAL      ELEV, CGAIN, COPAC, TS, TD, PR, NH2O, ZA, HA, AZ,
     *   COPACS(MAXIF), AZO(MAXIF), DGAIN(4), OOPAC
      LOGICAL   NEW
      INCLUDE 'INCS:PSTD.INC'
      REAL      DBGAI1(4,MXANT+1), DBGAI2(4,MXANT+1), DBGAI3(4,MXANT+1),
     *   DBGAI4(4,MXANT+1), DBGAI5(4,MXANT+1)
      EQUIVALENCE (DBGAI1, AGAINS(1,1,8,1))
      EQUIVALENCE (DBGAI2, AGAINS(1,1,8,2))
      EQUIVALENCE (DBGAI3, AGAINS(1,1,8,3))
      EQUIVALENCE (DBGAI4, AGAINS(1,1,8,4))
      EQUIVALENCE (DBGAI5, AGAINS(1,1,8,5))
      SAVE OOPAC
      DATA OOPAC /-1/
C-----------------------------------------------------------------------
      CGAIN = 1.0
      COPAC = 1.0
      IBAND = IBANDF(FQID)
      NEW = (OPACIT.EQ.2) .OR. (OPACIT.EQ.3)
C                                       find elevation
      CALL OSUELV (CALTBL, ANT, SUB, SRC, TIME, HA, ELEV, AZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Opacity
      IF (OPACIT.GT.0) THEN
C                                       Weather
         IF (OPACIT.GE.2) THEN
            NTFAIL(1) = NTFAIL(1) + 1
            IF (((TIME.LT.TIME1) .OR. (TIME.GT.TIME2)) .AND.
     *         ((OPACIT.EQ.2) .OR. (OPACIT.EQ.4))) THEN
               CALL INDWXG (WXTBL, SUB, TIME, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            IF ((TIME2.GT.TIME1) .AND. (TIME2.GE.TIME) .AND.
     *         (TIME.GE.TIME1)) THEN
               TS = TS1 + (TIME-TIME1)*(TS2-TS1)/(TIME2-TIME1)
               TD = TD1 + (TIME-TIME1)*(TD2-TD1)/(TIME2-TIME1)
               PR = PR1 + (TIME-TIME1)*(PR2-PR1)/(TIME2-TIME1)
            ELSE
               TS = TS1
               TD = TD1
               PR = PR1
               NTFAIL(2) = NTFAIL(2) + 1
               END IF
C                                       old method
            IF (.NOT.NEW) THEN
               CALL OPACTY (IBAND, TS, TD, PR, WTOPAC, MJD, TIME, NH2O,
     *            ZOPAC)
C                                       new method uses K band
            ELSE
               CALL OPACTY (8, TS, TD, PR, WTOPAC, MJD, TIME, NH2O,
     *            ZOPAC)
               CALL KBOPAC (NIF, FREQS(1,FQID), ZOPAC, ELEV, AZO,
     *            COPACS)
               END IF
C                                       only print if delta tau0 > 0.1%
            IF (ABS(ZOPAC-OOPAC).GE.0.0015) THEN
               IF (OPACIT.EQ.2) THEN
                  WRITE (MSGTXT,1000) AZO(1), ZOPAC
               ELSE
                  WRITE (MSGTXT,1001) ZOPAC
                  END IF
               CALL MSGWRT (3)
               OOPAC = ZOPAC
               END IF
            END IF
         COPAC = ZOPAC / SIN (ELEV)
         COPAC = SQRT (EXP (COPAC))
         END IF
C                                       antenna gain
      IF (GNTYP.GT.0) THEN
         IF (GNTYP.EQ.2) THEN
            WANT = MXANT+1
         ELSE
            WANT = ANT
            END IF
         ZA = 90.0 * (1. - 2. * ELEV / PI)
         DO 50 I = 1,MAX(1,NFGAIN(IBAND))
            DGAIN(I) = AGAINS(1,WANT,IBAND,I) +
     *         ZA*AGAINS(2,WANT,IBAND,I)
     *         + ZA*ZA * AGAINS(3,WANT,IBAND,I)
     *         + ZA*ZA*ZA * AGAINS(4,WANT,IBAND,I)
            IF (DGAIN(I).LE.0.0) THEN
               WANT = MXANT + 1
               DGAIN(I) = AGAINS(1,WANT,IBAND,I) +
     *            ZA*AGAINS(2,WANT,IBAND,I) +
     *            ZA*ZA * AGAINS(3,WANT,IBAND,I)+
     *            ZA*ZA*ZA * AGAINS(4,WANT,IBAND,I)
               END IF
            IF (DGAIN(I).LE.0.0) DGAIN(I) = 1.0
 50         CONTINUE
C                                       The correction for the gain is
C                                       1 over the gain curve value.
         CGAIN = 1.0 / DGAIN(1)
         END IF
C                                       apply to CL
      IF ((GNTYP.LE.0) .OR. (.NOT.EVLA) .OR. (NFGAIN(IBAND).LE.1)) THEN
         DO 60 IPOL = 1,NUMPOL
            DO 55 IIF = 1,NUMIF
               IF (NEW) THEN
                  CREAL(IPOL,IIF) = CGAIN * COPACS(IIF)
               ELSE
                  CREAL(IPOL,IIF) = CGAIN * COPAC
                  END IF
 55            CONTINUE
 60         CONTINUE
C                                       freq dependent
      ELSE
         DO 80 IIF = 1,NUMIF
            CGAIN = 0.
            NFG = NFGAIN(IBAND)
            IF (NFG.EQ.1) THEN
               CGAIN = DGAIN(1)
            ELSE IF (FREQS(IIF,FQID).LE.FGAINS(IBAND,1)) THEN
               CGAIN = DGAIN(1) + (DGAIN(2) - DGAIN(1)) *
     *            (FREQS(IIF,FQID) - FGAINS(IBAND,1)) /
     *            (FGAINS(IBAND,2) - FGAINS(IBAND,1))
            ELSE IF (FREQS(IIF,FQID).GE.FGAINS(IBAND,NFG)) THEN
               I = NFG - 1
               CGAIN = DGAIN(I) + (DGAIN(I+1) - DGAIN(I)) *
     *            (FREQS(IIF,FQID) - FGAINS(IBAND,I)) /
     *            (FGAINS(IBAND,I+1) - FGAINS(IBAND,I))
            ELSE
               DO 70 I = 1,NFG-1
                  IF ((FREQS(IIF,FQID).GE.FGAINS(IBAND,I)) .AND.
     *               (FREQS(IIF,FQID).LT.FGAINS(IBAND,I+1)))
     *               CGAIN = DGAIN(I) + (DGAIN(I+1) - DGAIN(I)) *
     *               (FREQS(IIF,FQID) - FGAINS(IBAND,I)) /
     *               (FGAINS(IBAND,I+1) - FGAINS(IBAND,I))
 70               CONTINUE
               END IF
            IF (CGAIN.EQ.0.0) CGAIN = 1.0
            CGAIN = 1.0 / CGAIN
            DO 75 IPOL = 1,NUMPOL
               IF (NEW) THEN
                  CREAL(IPOL,IIF) = CGAIN * COPACS(IIF)
               ELSE
                  CREAL(IPOL,IIF) = CGAIN * COPAC
                  END IF
 75            CONTINUE
 80         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Zenith opacity of IF 1 set to ',F6.4,' (',F6.4,
     *   ' at K-band)')
 1001 FORMAT ('Zenith opacity set to ',F6.4)
      END
      SUBROUTINE INDWXG (WXTBL, SUB, TIME, IRET)
C-----------------------------------------------------------------------
C   Read the WX table to get the two records spanning TIME.
C   Assume time order mostly
C   Inputs:
C      SUB     I       Desired subarray
C      TIME    D       Desired time
C   In/out:
C      WXTBL   C*(*)   WX table object
C   Outputs:
C      IRET    I       Error code
C-----------------------------------------------------------------------
      CHARACTER WXTBL*(*)
      INTEGER   SUB, IRET
      DOUBLE PRECISION TIME
C
      INTEGER   IANT, ISUB
      REAL      DTIME, WV, WD, WGUST, PRECIP, H2O, ION
      INCLUDE 'VLAGN.INC'
C-----------------------------------------------------------------------
      IF (TIME.LT.TIME1) THEN
         WXROW = 1
         TIME1 = -1.E5
         TIME2 = -1.E5
         END IF
      IRET = 0
 10   IF ((TIME.GT.TIME2) .AND. (WXROW.LE.NWXROW)) THEN
         TIME1 = TIME2
         TS1 = TS2
         TD1 = TD2
         PR1 = PR2
         CALL OTABWX (WXTBL, 'READ', WXROW, TIME2, DTIME, IANT, ISUB,
     *      TS2, PR2, TD2, WV, WD, WGUST, PRECIP, H2O, ION, IRET)
         IF (IRET.EQ.0) THEN
C                                       find accepatble subarray
 20         IF ((ISUB.EQ.SUB) .OR. (ISUB.EQ.0) .OR. (SUB.EQ.0)) GO TO 10
            CALL OTABWX (WXTBL, 'READ', WXROW, TIME2, DTIME, IANT, ISUB,
     *         TS2, PR2, TD2, WV, WD, WGUST, PRECIP, H2O, ION, IRET)
            IF (IRET.EQ.0) GO TO 20
            END IF
         END IF
C
 999  RETURN
      END

LOCAL INCLUDE 'FIXWT.INC'
      REAL      SOLINT, AVG, LIMITS(10), WTMAX
      INTEGER   CCHAN, CIF, CANT, CPOL, FLAGS, DOALL
      COMMON /FXWT/ SOLINT, CCHAN, CIF, CANT, CPOL, DOALL, AVG, FLAGS,
     *   LIMITS, WTMAX
LOCAL END
LOCAL INCLUDE 'XXTAB.INC'
C                                       Column indices
C                                         (1) time
C                                         (2) baseline
C                                         (3) weight
      INTEGER   COLIDX(3), NWEIGH
C
      COMMON /TABXX/ COLIDX, NWEIGH
LOCAL END
      PROGRAM FIXWT
C-----------------------------------------------------------------------
C! Task to fix uv data weights based on amplitude std. deviations
C# UV Calibration OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2003, 2005, 2012, 2015
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
C   Select data for a single channel and IF and sort into BT order.
C   Run a filter of width 2 * XINC + 1 over the data for each baseline,
C   breaking at source changes, frequency ID changes and gaps longer
C   than the filter half-width and calculate new weights using a MAD
C   estimate of the noise; weights are replicated for data points at
C   the beginning and end of a filtered subsection.  Sort the weights
C   into TB order and merge them into the original data.
C-----------------------------------------------------------------------
C                                       Inputs
      CHARACTER INPUTS*6
      PARAMETER (INPUTS = 'Inputs')
C                                       Original data
      CHARACTER IDATA*15
      PARAMETER (IDATA = 'Input Data File')
C                                       Weights scratch file
      CHARACTER WTFILE*12
      PARAMETER (WTFILE = 'Weights File')
C                                       Output data file
      CHARACTER OUTFIL*11
      PARAMETER (OUTFIL = 'Output File')
C                                       Return status
      INTEGER   IRET, IERR
C                                       Scratch buffer, sum arrays
      INTEGER   BUFFER(256), NWORDS, COUNT(2)
      LONGINT   COFF, SOFF, SQOFF, WOFF, SWOFF
      REAL      SUM(2), SUMSQ(2), SUMWT(2), WEIGHT(2)
C
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FIXWT.INC'
      INCLUDE 'XXTAB.INC'
C-----------------------------------------------------------------------
C                                       Initialize task and read inputs:
      CALL FIXWIN (INPUTS, IDATA, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Prepare summing arrays
      NWEIGH = CPOL * CIF * CANT
      NWORDS = (CANT * NWEIGH - 1) / 1024 + 1
      NWEIGH = (NWEIGH - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'FIXWT', NWORDS, SUM, SOFF, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FIXWT', NWORDS, SUMSQ, SQOFF,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FIXWT', NWORDS, COUNT, COFF,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FIXWT', NWORDS, SUMWT, SWOFF,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FIXWT', NWEIGH, WEIGHT,
     *   WOFF, IRET)
      IF (IRET.NE.0) GO TO 900
      NWEIGH = NWEIGH * 1024
C                                       Calculate weights:
      MSGTXT = 'Calculating weights'
      CALL MSGWRT (4)
      CALL CALCWT (IDATA, WTFILE, CPOL, CIF, CANT, SUM(1+SOFF),
     *   SUMSQ(1+SQOFF), SUMWT(1+SWOFF), COUNT(1+COFF), WEIGHT(1+WOFF),
     *   IRET)
      IF (IRET.NE.0) GO TO 900
      CALL ZMEMRY ('FREE', 'FIXWT', NWORDS, SUM, SOFF, IERR)
      CALL ZMEMRY ('FREE', 'FIXWT', NWORDS, SUMSQ, SQOFF, IERR)
      CALL ZMEMRY ('FREE', 'FIXWT', NWORDS, COUNT, COFF, IERR)
      CALL ZMEMRY ('FREE', 'FIXWT', NWORDS, SUMWT, SWOFF, IERR)
C                                       Apply weights:
      IF (DOALL.GT.0) THEN
         MSGTXT = 'Averaging weights for re-scale factor'
         CALL MSGWRT (4)
         CALL AVGWTS (IDATA, WTFILE, CPOL, CIF, CANT, WEIGHT(1+WOFF),
     *      IRET)
         IF (IRET.NE.0.0) GO TO 900
         END IF
C                                       Apply weights:
      MSGTXT = 'Merging weights into data'
      CALL MSGWRT (4)
      CALL MRGWTS (INPUTS, IDATA, WTFILE, CIF, CANT, OUTFIL,
     *   WEIGHT(1+WOFF), IRET)
      NWEIGH = NWEIGH / 1024
      CALL ZMEMRY ('FREE', 'FIXWT', NWEIGH, WEIGHT, WOFF, IERR)
      IF (IRET.NE.0) GO TO 900
C                                       Update history file:
      CALL FWTHIS (INPUTS, IDATA, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Finish task:
 900  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE FIXWIN (INPUTS, IDATA, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Initialize the FIXWT task and read its input adverbs.
C   Inputs:
C      INPUTS   C*(*)   Name of inputs object
C      IDATA    C*(*)   Name of UVDATA object for input file
C      OUTFIL   C*(*)   Name of UVDATA object for output file
C   Outputs:
C      IRET     I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), IDATA*(*), OUTFIL*(*)
      INTEGER   IRET
C                                       Reference channel number.
      INTEGER   BCHAN, ECHAN
C                                       Reference IF number.
      INTEGER   BIF, EIF
C                                       Maximum channel number.
      INTEGER   MXCHNO
C                                       Maximum IF number (-1 if no IF
C                                       axis is present).
      INTEGER   MXIFNO
C                                       Input data sort order
      CHARACTER SRTORD*2
C                                       Task name.
      CHARACTER TNAME*6
      PARAMETER (TNAME = 'FIXWT ')
C                                       Number of adverbs.
      INTEGER   NUMADV
      PARAMETER (NUMADV = 16)
C                                       Adverb names.
      CHARACTER AVNAME(NUMADV)*8
C                                       Adverb types.
      INTEGER   AVTYPE(NUMADV)
C                                       Adverb dimensions.
      INTEGER   AVDIM(2,NUMADV)
C                                       Number of adverb values to
C                                       transfer directly to UVDATA
C                                       object.
      INTEGER   NKEY
      PARAMETER (NKEY = 4)
C                                       Adverb values to transfer to
C                                       the input and output file and
C                                       their corresponding attributes.
      CHARACTER INKEY(NKEY)*8, INKEY2(NKEY)*8, OUTKEY(NKEY)*32
C                                       Input file name, output file
C                                       name and output file class:
      CHARACTER INNAME*12, OUTNAM*12, OUTCLA*6, TABLE*30
C                                       Data array dimensions
      INTEGER   NAXIS(16)
C                                       Object attribute type
      INTEGER   TYPE
C                                       Object attribute dimensions
      INTEGER   DIM(3)
C                                       Axis index
      INTEGER   INDEX
      LOGICAL   CMPRS
C
      CHARACTER DUMMY*1, UVTYPE*2, STOKES*4
      INTEGER   IDUMMY, FGVER, ANVER, MXA
      LOGICAL   EXISTS
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FIXWT.INC'
C
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
     *             'DOUVCOMP', 'FLAGVER ', 'BCHAN   ', 'ECHAN   ',
     *             'SOLINT  ', 'DOALL   ', 'CPARM   ','BADDISK '/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *             OOACAR, OOACAR, OOAINT, OOAINT,
     *             OOALOG, OOAINT, OOAINT, OOAINT,
     *             OOARE,  OOAINT, OOARE,  OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10,1, 10,1/
      DATA INKEY /'INNAME', 'INCLASS', 'INSEQ', 'INDISK'/
      DATA INKEY2 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
      DATA OUTKEY /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Initialize the task and read
C                                       the input adverb values:
      CALL AV2INP (TNAME, NUMADV, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create a UVDATA object for the
C                                       input data:
      CALL OUVCRE (IDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVCRE (OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Transfer file name information
C                                       to the input file:
      CALL IN2OBJ (INPUTS, NKEY, INKEY, OUTKEY, IDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open the data file and obtain
C                                       the number of channels and IFs:
      DIM(1) = 4
      DIM(2) = 1
      STOKES = ' '
      CALL OPUT (IDATA, 'STOKES', OOACAR, DIM, IDUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVOPN (IDATA, 'READ', IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO OPEN INPUT FILE'
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      CALL UVDGET (IDATA, 'NAXIS', TYPE, DIM, NAXIS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (IDATA, 2, 'STOKES', INDEX, IRET)
      IF (IRET.NE.0) GO TO 999
      CPOL = NAXIS(INDEX)
      CPOL = MIN (2, CPOL)
      CALL UVDFND (IDATA, 2, 'FREQ', INDEX, IRET)
      IF (IRET.NE.0) GO TO 999
      MXCHNO = NAXIS(INDEX)
      CALL UVDFND (IDATA, 2, 'IF', INDEX, IRET)
      IF ((IRET.NE.0).AND.(IRET.NE.1)) GO TO 999
C                                       IF axis is present.
      IF (IRET.EQ.0) THEN
         MXIFNO = NAXIS(INDEX)
C                                       No IF axis is present.
      ELSE
         MXIFNO = 1
         END IF
C                                       Check the sort order: the merge
C                                       step will fail if the data are
C                                       not in TB order.
      CALL UVDGET (IDATA, 'SORTORD', TYPE, DIM, IDUMMY, SRTORD, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SRTORD(:2).NE.'TB') THEN
         MSGTXT = 'INPUT DATA IS NOT IN ''TB'' ORDER'
         CALL MSGWRT (9)
         MSGTXT = 'RUN UVSRT WITH SORT = ''TB'' FIRST'
         CALL MSGWRT (9)
         IRET = 1
         GO TO 999
         END IF
C                                       check compression - SB not allow
      CALL UVDGET (IDATA, 'TYPEUVD', TYPE, DIM, IDUMMY, UVTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'DOUVCOMP', TYPE, DIM, CMPRS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (UVTYPE.EQ.'SB') CMPRS = .FALSE.
      CALL INPUTT (INPUTS, 'DOUVCOMP', TYPE, DIM, CMPRS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'CPARM', TYPE, DIM, LIMITS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (LIMITS(2).LE.0.0) LIMITS(2) = 1.E20
      IF (LIMITS(4).LE.0.0) LIMITS(4) = 1.E20
      IF (LIMITS(2).LE.LIMITS(1)) LIMITS(2) = 1.E20
      IF (LIMITS(4).LE.LIMITS(3)) LIMITS(4) = 1.E20
      LIMITS(5) = MAX (0.0, MIN (1.0, LIMITS(5)))
      CALL OPUT (INPUTS, 'CPARM', TYPE, DIM, LIMITS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      LIMITS(1) = LIMITS(1) * LIMITS(1)
      LIMITS(2) = LIMITS(2) * LIMITS(2)
C                                       close uv object
      CALL OUVCLO (IDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Fill in defaults for the output
C                                       file name if needed:
      CALL FNAGET (IDATA, 'NAME', TYPE, DIM, IDUMMY, INNAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'OUTNAME', TYPE, DIM, IDUMMY, OUTNAM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OUTNAM.EQ.' ') THEN
         CALL INPUTT (INPUTS, 'OUTNAME', TYPE, DIM, IDUMMY, INNAME,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL INGET (INPUTS, 'OUTCLASS', TYPE, DIM, IDUMMY, OUTCLA, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OUTCLA.EQ.' ') THEN
         CALL INPUTT (INPUTS, 'OUTCLASS', TYPE, DIM, IDUMMY, 'FIXWT ',
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Transfer output file name
C                                       adverbs to output file:
      CALL IN2OBJ (INPUTS, NKEY, INKEY2, OUTKEY, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Select the channel range
      CALL INGET (INPUTS, 'DOALL', TYPE, DIM, DOALL, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'BCHAN', TYPE, DIM, BCHAN, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (INPUTS, 'ECHAN', TYPE, DIM, ECHAN, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      BCHAN = MAX (BCHAN, 1)
      IF (ECHAN.LT.BCHAN) ECHAN = MXCHNO
      IF (ECHAN.LT.BCHAN) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (4)
         BCHAN = 1
         END IF
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL SECPUT (IDATA, 'BCHAN', TYPE, DIM, BCHAN, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECPUT (IDATA, 'ECHAN', TYPE, DIM, ECHAN, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CCHAN = ECHAN - BCHAN + 1
C                                       Change all IFs
      BIF = 1
      EIF = MXIFNO
      CIF = EIF -  BIF + 1
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL SECPUT (IDATA, 'BIF', TYPE, DIM, BIF, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECPUT (IDATA, 'EIF', TYPE, DIM, EIF, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CIF = EIF - BIF + 1
C                                       Turn off flagging for input
C                                       file:
      CALL INGET (INPUTS, 'FLAGVER', TYPE, DIM, FGVER, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL SECPUT (IDATA, 'FGVER', TYPE, DIM, FGVER, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get the filter width:
      CALL INGET (INPUTS, 'SOLINT', TYPE, DIM, SOLINT, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SOLINT.LE.0.0) SOLINT = 1.0
      SOLINT = SOLINT / (24.0 * 60.0)
C                                       Get bad disk numbers:
      CALL INGET (INPUTS, 'BADDISK', TYPE, DIM, IBAD, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       max ant no
      TABLE = 'Temporary AN table for FIXWT'
      CANT = 0
      DO 20 ANVER = 1,99
         CALL UV2TAB (IDATA, TABLE, 'AN', ANVER, IRET)
         EXISTS = .FALSE.
         IF (IRET.EQ.0) CALL TABEXI (TABLE, EXISTS, IRET)
         MXA = 0
         IF ((IRET.EQ.0) .AND. (EXISTS)) CALL ANTNO (TABLE, ANVER, MXA,
     *      IRET)
         CALL TABDES (TABLE, IDUMMY)
         IF ((IRET.EQ.0) .AND. (EXISTS)) THEN
            CANT = MAX (CANT, MXA)
         ELSE
            GO TO 999
            END IF
 20      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Changing begin channel number to 1')
 1002 FORMAT ('Changing filter half-width (XINC) to ', I2,
     *   ' data points')
      END
      SUBROUTINE CALCWT (IDATA, WTFILE, NPOL, NIF, NANT, SUM, SUMSQ,
     *   SUMWT, COUNT, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   Calculate new weights and write them to a second scratch file,
C   deleting the first on completion.
C   Inputs:
C      IDATA    C*(*)   Name of object for input data
C      WTFILE   C*(*)   Name of object for weights file
C   Output:
C      IRET     I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER IDATA*(*), WTFILE*(*)
      INTEGER   NPOL, NIF, NANT, COUNT(NANT,NANT,NIF,NPOL), IRET
      REAL      SUM(NANT,NANT,NIF,NPOL), SUMSQ(NANT,NANT,NIF,NPOL),
     *   SUMWT(NANT,NANT,NIF,NPOL), WEIGHT(NANT,NIF,NPOL)
C
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA
C                                       Numbers of data axes
      INTEGER   JLOCC, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF
C                                       Increments in data
      INTEGER   INCS, INCF, INCIF
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FIXWT.INC'
      INTEGER   IANT, JANT, LCHAN, LP, LIF, LASTSU, LASTFQ, IP, XXROW
      REAL      TB, TE, RP(20), VIS(3,MAXCIF), TEMP
      LOGICAL   FINISH, RESTRT
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Retrieve pointers into the data:
C                                       pointers into the data:
      CALL UVDPNT (IDATA, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IRET)
      IF (IRET.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Check that the COMPLEX axis
C                                       comes first, as assumed:
      IF (JLOCC.NE.1) THEN
         MSGTXT = 'COMPLEX AXIS IS NOT FIRST AXIS - TOO WIERD FOR ME'
         CALL MSGWRT (10)
         IRET = 1
         GO TO 999
         END IF
C                                       Create and open a scratch table:
      CALL UV2TAB (IDATA, WTFILE, 'XX', 1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OXXINI (WTFILE, 'WRIT', XXROW, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open the data file:
      CALL OUVOPN (IDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      TB = 0.0
      TE = -10000.
      LASTSU = -100
      LASTFQ = -100
      WTMAX = 0.0
C                                       Start the filter:
 10   CONTINUE
         CALL UVREAD (IDATA, RP, VIS, IRET)
         IF (IRET.EQ.-1) THEN
            FINISH = .TRUE.
            IRET = 0
            END IF
         IF (IRET.NE.0) GO TO 999
         RESTRT = FINISH
         IF (.NOT.FINISH) THEN
            RESTRT = RP(ILOCT).GT.TE
            IF (ILOCSU.GT.0) THEN
               IF (RP(ILOCSU).NE.LASTSU) RESTRT = .TRUE.
               END IF
            IF (ILOCFQ.GT.0) THEN
               IF (RP(ILOCFQ).NE.LASTFQ) RESTRT = .TRUE.
               END IF
            END IF
C                                       interval is done
         IF (RESTRT) THEN
C                                       have data ready to analyze
            IF (TB.LT.TE) THEN
               DO 30 LP = 1,NPOL
                  DO 25 LIF = 1,NIF
                     DO 20 JANT = 1,NANT
                        DO 15 IANT = 1,NANT
                           IF (COUNT(IANT,JANT,LIF,LP).LT.5) THEN
                              SUMSQ(IANT,JANT,LIF,LP) = FBLANK
                           ELSE
                              TEMP =  SUM(IANT,JANT,LIF,LP) /
     *                           SUMWT(IANT,JANT,LIF,LP)
                              SUMSQ(IANT,JANT,LIF,LP) =
     *                           SUMSQ(IANT,JANT,LIF,LP) /
     *                           SUMWT(IANT,JANT,LIF,LP) - TEMP*TEMP
                              END IF
 15                        CONTINUE
 20                     CONTINUE
                     CALL WTSOLV (NANT, SUMSQ(1,1,LIF,LP), LIMITS,
     *                  WEIGHT(1,LIF,LP))
 25                  CONTINUE
 30               CONTINUE
               CALL OTABXX (WTFILE, 'WRIT', XXROW, TB, TE, WEIGHT, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       zero summing arrays
            DO 50 LP = 1,NPOL
               DO 45 LIF = 1,NIF
                  DO 40 JANT = 1,NANT
                     WEIGHT(JANT,LIF,LP) = FBLANK
                     DO 35 IANT = 1,NANT
                        COUNT(IANT,JANT,LIF,LP) = 0
                        SUM(IANT,JANT,LIF,LP) = 0.0
                        SUMWT(IANT,JANT,LIF,LP) = 0.0
                        SUMSQ(IANT,JANT,LIF,LP) = 0.0
 35                     CONTINUE
 40                  CONTINUE
 45               CONTINUE
 50            CONTINUE
            TB = RP(ILOCT)
            TE = TB + SOLINT
            IF (ILOCSU.GT.0) LASTSU = RP(ILOCSU)
            IF (ILOCFQ.GT.0) LASTFQ = RP(ILOCFQ)
            END IF
C                                       add in record
         IF (.NOT.FINISH) THEN
            IF (ILOCB.GT.0) THEN
               JANT = RP(ILOCB) + 0.001
               IANT = JANT / 256
               JANT = JANT - 256 * IANT
            ELSE
               IANT = RP(ILOCA1) + 0.1
               JANT = RP(ILOCA2) + 0.1
               END IF
            DO 70 LP = 1,NPOL
               DO 65 LIF = 1,NIF
                  IP = (LP-1) * INCS + (LIF-1) * INCIF - INCF + 1
                  DO 60 LCHAN = 1,CCHAN
                     IP = IP + INCF
C                                       real part in I < J
                     IF (VIS(3,IP).GT.0.0) THEN
                        WTMAX = MAX (WTMAX, VIS(3,IP))
                        COUNT(IANT,JANT,LIF,LP) =
     *                     COUNT(IANT,JANT,LIF,LP) + 1
                        SUM(IANT,JANT,LIF,LP) = SUM(IANT,JANT,LIF,LP) +
     *                     VIS(1,IP)*VIS(3,IP)
                        SUMSQ(IANT,JANT,LIF,LP) =
     *                     SUMSQ(IANT,JANT,LIF,LP) +
     *                     VIS(1,IP)*VIS(1,IP)*VIS(3,IP)
                        SUMWT(IANT,JANT,LIF,LP) =
     *                     SUMWT(IANT,JANT,LIF,LP) + VIS(3,IP)
                        END IF
C                                       imag part in I > J
                     IF ((VIS(3,IP).GT.0.0) .AND. (IANT.NE.JANT)) THEN
                        COUNT(JANT,IANT,LIF,LP) =
     *                     COUNT(JANT,IANT,LIF,LP) + 1
                        SUM(JANT,IANT,LIF,LP) = SUM(JANT,IANT,LIF,LP) +
     *                     VIS(2,IP)*VIS(3,IP)
                        SUMSQ(JANT,IANT,LIF,LP) =
     *                     SUMSQ(JANT,IANT,LIF,LP) +
     *                     VIS(2,IP)*VIS(2,IP)*VIS(3,IP)
                        SUMWT(JANT,IANT,LIF,LP) =
     *                     SUMWT(JANT,IANT,LIF,LP) + VIS(3,IP)
                        END IF
 60                  CONTINUE
 65               CONTINUE
 70            CONTINUE
C                                       loop for more data
            GO TO 10
            END IF
C                                       Close files:
      CALL TABCLO (WTFILE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVCLO (IDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1070) WTMAX
      CALL MSGWRT (4)
C
  999 RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('Maximum data weight read =',1PE12.4)
      END
      SUBROUTINE AVGWTS (IDATA, WTFILE, NPOL, NIF, NANT, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   Compare new weights to old and get scaling factors
C   Inputs:
C      IDATA      C*(*)   Name of UVDATA object for input data
C      WTFILE     C*(*)   Name of TABLE object for weights file
C      NPOL       I       Number polarizations
C      NIF        I       Number IFs
C      NANT       I       Number antennas
C   Output:
C      WEIGHT     R(*)    Work buffer
C      IRET       I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER WTFILE*(*), IDATA*(*)
      INTEGER   NPOL, NIF, NANT, IRET
      REAL      WEIGHT(NANT,NIF,NPOL)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FIXWT.INC'
      REAL      TB, TE, RP(20), VIS(3,MAXCIF)
      INTEGER   XXROW, NGROUP
C                                       Pointers to random parameters
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR, JLOCD,
     *   JLOCIF, INCS, INCF, INCIF
C                                       Counters
      INTEGER   DIM(3), TYPE, NFLAG, NSKIP, COUNT, IP, IANT, JANT, LP,
     *   LIF, LCHAN, ITER
      CHARACTER DUMMY*1
      REAL      WT, PRVTIM, AVGSQ, V, VAVG, VDIF, SUMWT
C                                       DEBUG
      INTEGER   HISTO(1000), IH
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      VAVG = 1.0
      VDIF = 100000.
C                                       Self-consistent average
      DO 100 ITER = 1,8
         NFLAG = 0
         NSKIP = 0
         CALL OUVOPN (IDATA, 'READ', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OXXINI (WTFILE, 'READ', XXROW, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get axis sizes and pointers:
         CALL UVDPNT (IDATA, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *      ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *      JLOCD, JLOCIF, INCS, INCF, INCIF, IRET)
         IF (IRET.NE.0) GO TO 999
         INCS = INCS / 3
         INCF = INCF / 3
         INCIF = INCIF / 3
         CALL TABGET (WTFILE, 'NROW', TYPE, DIM, NGROUP, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         TE = -1.E10
         PRVTIM = TE
         COUNT = 0
         AVG = 0.0
         AVGSQ = 0.0
         SUMWT = 0.0
         CALL FILL (1000, 0, HISTO)
C                                       Read loop
 10      CALL UVREAD (IDATA, RP, VIS, IRET)
         IF (IRET.GT.0) THEN
            GO TO 999
         ELSE IF (IRET.EQ.0) THEN
C                                       Check for bad sort order.
            IF (RP(ILOCT).LT.PRVTIM)THEN
               MSGTXT='INCORRECT SORT ORDER GIVEN IN INPUT FILE HEADER'
               CALL MSGWRT (8)
               MSGTXT = 'DATA IS NOT IN T* ORDER --- ABORTING'
               CALL MSGWRT (8)
               MSGTXT = 'RUN UVSRT WITH SORT = ''TB'' AND TRY AGAIN'
               CALL MSGWRT (8)
               IRET = 1
               GO TO 999
               END IF
            PRVTIM = RP(ILOCT)
            IF (RP(ILOCT).GT.TE) THEN
               IF (XXROW.GT.NGROUP) THEN
                  MSGTXT = 'AVGWTS: READ OF END OF WT TABLE'
                  CALL MSGWRT (8)
                  IRET = 1
                  GO TO 999
                  END IF
               CALL OTABXX (WTFILE, 'READ', XXROW, TB, TE, WEIGHT, IRET)
               IF (IRET.NE.0) GO TO 999
               IF (RP(ILOCT).LT.TB) THEN
                  NSKIP = NSKIP + 1
                  GO TO 10
                  END IF
               END IF
            IF (ILOCB.GT.0) THEN
               JANT = RP(ILOCB) + 0.001
               IANT = JANT / 256
               JANT = JANT - 256 * IANT
            ELSE
               IANT = RP(ILOCA1) + 0.1
               JANT = RP(ILOCA2) + 0.1
               END IF
            DO 50 LP = 1,NPOL
               DO 40 LIF = 1,NIF
                  IP = (LP-1) * INCS + (LIF-1) * INCIF - INCF + 1
                  WT = WEIGHT(IANT,LIF,LP) * WEIGHT(JANT,LIF,LP)
                  IF (WT.LE.0) THEN
C                                       were any data good?
                     DO 25 LCHAN = 1,CCHAN
                        IP = IP + INCF
                        IF (VIS(3,IP).GT.0.0) THEN
                           NFLAG = NFLAG + 1
                           GO TO 40
                           END IF
 25                     CONTINUE
                  ELSE
                     DO 30 LCHAN = 1,CCHAN
                        IP = IP + INCF
C                                       average
                        IF (VIS(3,IP).GT.0.0) THEN
                           V = SQRT (VIS(3,IP) / WT)
                           IF ((ITER.EQ.1) .OR. (ABS(V-VAVG).LT.VDIF))
     *                        THEN
                              IH = 200*V + 1.5
                              IH = MAX (1, MIN (1000, IH))
                              HISTO(IH) = HISTO(IH) + 1
                              AVG = AVG + V * VIS(3,IP)
                              AVGSQ = AVGSQ + V * V * VIS(3,IP)
                              COUNT = COUNT + 1
                              SUMWT = SUMWT + VIS(3,IP)
                              END IF
                           END IF
 30                     CONTINUE
                     END IF
 40               CONTINUE
 50            CONTINUE
            GO TO 10
            END IF
C                                       Close files:
         CALL OUVCLO (IDATA, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABCLO (WTFILE, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (COUNT.GT.0) THEN
            AVG = AVG / SUMWT
            AVGSQ = AVGSQ / SUMWT - AVG*AVG
            AVGSQ = SQRT (MAX (0.0, AVGSQ))
            WRITE (MSGTXT,1000) ITER, AVG, AVGSQ
            CALL MSGWRT (4)
            IF (ABS((AVG-VAVG)/AVG).LT.0.02) GO TO 110
            VAVG = AVG
            VDIF = 2.5 * AVGSQ
         ELSE
            GO TO 110
            END IF
 100     CONTINUE
C                                       EOF in data reached
 110  IF (XXROW.LE.NGROUP) THEN
         MSGTXT = 'AVGWTS: DID NOT READ ALL WEIGHT TABLE'
         CALL MSGWRT (7)
         END IF
C                                       Report problems
      WRITE (MSGTXT,1050) NSKIP
      IF (NSKIP.GT.0) CALL MSGWRT (6)
      WRITE (MSGTXT,1051) NFLAG
      IF (NFLAG.GT.0) CALL MSGWRT (6)
C                                       average weights
C                                       for 1/sqrt
      VAVG = 2.0 / (AVG*AVG*AVG) * AVGSQ
      AVGSQ = 2.0 * AVG * AVGSQ
      AVG = AVG * AVG
      WRITE (MSGTXT,1110) AVG, AVGSQ
      CALL MSGWRT (4)
      AVG = 1.0 / AVG
      WRITE (MSGTXT,1111) AVG, VAVG
      CALL MSGWRT (4)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVGWTS: ITER',I2,' avg rms fac',1PE12.4,' +-',1PE12.4)
 1050 FORMAT ('AVGWTS:',I7,
     *   ' visibilities fell between weight table rows')
 1051 FORMAT ('AVGWTS:',I7,' visibilities did not have table weights')
 1110 FORMAT ('AVGWTS: average rms**2 factor',1PE12.4,' +-',1PE12.4)
 1111 FORMAT ('AVGWTS: weights will be multiplied by',1PE12.4,
     *   ' +-',1PE12.4)
      END
      SUBROUTINE MRGWTS (INPUTS, IDATA, WTFILE, NIF, NANT, OUTFIL,
     *   WEIGHT, IRET)
C-----------------------------------------------------------------------
C   Merge original data with new weights and write the results to
C   OUTFIL, which should be already created.
C   Inputs:
C      INPUTS     C*(*)   Name of INPUTS object
C      IDATA      C*(*)   Name of UVDATA object for input data
C      WTFILE     C*(*)   Name of TABLE object for weights file
C      NIF        I       Number IFs
C      NANT       I       Number antennas
C      OUTFIL     C*(*)   Name of output UV data object
C   Output:
C      WEIGHT     R(*)    Work buffer
C      IRET       I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), WTFILE*(*), IDATA*(*), OUTFIL*(*)
      INTEGER   NIF, NANT, IRET
      REAL      WEIGHT(NANT,NIF,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FIXWT.INC'
      REAL      RP(20), VIS(3,MAXCIF), TB, TE, PRVTIM, WT
      INTEGER   XXROW, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, NGROUP, DIM(3), TYPE, IDUMMY,
     *   NFLAG, NSKIP, IP, IANT, JANT, LP, LIF, LCHAN, NCHAN, NAXIS(7),
     *   COUNT, NPOL, IA(4), JB(4)
      LOGICAL   COMPRS, ONEOK, DOMSG1, DOMSG3
      CHARACTER DUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IA, JB /1,2,1,2, 1,2,2,1/
C-----------------------------------------------------------------------
      DOMSG1 = .TRUE.
      DOMSG3 = .TRUE.
      COUNT = 0
      FLAGS = 0
C                                       full channel selection
      CALL SECINI (IDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Flagging must be off for input
C                                       file.
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 1
      CALL SECPUT (IDATA, 'FGVER', TYPE, DIM, -1, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NFLAG = 0
      NSKIP = 0
C                                       Open files:
      CALL OUVOPN (IDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OXXINI (WTFILE, 'READ', XXROW, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDCOP (IDATA, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set compression
      CALL INGET (INPUTS, 'DOUVCOMP', TYPE, DIM, COMPRS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDPUT (OUTFIL, 'ISCOMP', TYPE, DIM, COMPRS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
      CALL OUVOPN (OUTFIL, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get axis sizes and pointers:
      CALL UVDPNT (IDATA, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *  JLOCD, JLOCIF, INCS, INCF, INCIF, IRET)
      IF (IRET.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
      CALL TABGET (WTFILE, 'NROW', TYPE, DIM, NGROUP, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (IDATA, 'NAXIS', TYPE, DIM, NAXIS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NCHAN = NAXIS(JLOCF)
      NPOL = NAXIS(JLOCS)
C                                       if overall scale, need no times
C                                       or table here
      PRVTIM = -9.99E+10
      IF (DOALL.GT.0) THEN
         TE = 1.E10
         TB = -TE
C                                       else set up for reading table
      ELSE
         TE = -1.E10
         END IF
C                                       Merge data loop
 10   CALL UVREAD (IDATA, RP, VIS, IRET)
      IF (IRET.GT.0) THEN
         GO TO 999
      ELSE IF (IRET.EQ.0) THEN
C                                       Check for bad sort order.
         IF (RP(ILOCT).LT.PRVTIM) THEN
            IF (DOMSG1) THEN
               MSGTXT = 'INCORRECT SORT ORDER IN INPUT FILE HEADER'
               CALL MSGWRT (8)
               MSGTXT = 'DATA IS NOT IN T* ORDER --- SKIPPING'
               CALL MSGWRT (8)
               DOMSG1 = .FALSE.
               END IF
            NSKIP = NSKIP + 1
            GO TO 10
            END IF
         PRVTIM = RP(ILOCT)
         IF (RP(ILOCT).GT.TE) THEN
            IF (XXROW.GT.NGROUP) THEN
               MSGTXT = 'AVGWTS: READ OF END OF WT TABLE'
               IF (DOMSG3) CALL MSGWRT (8)
               DOMSG3 = .FALSE.
               NSKIP = NSKIP + 1
               GO TO 10
            ELSE
               CALL OTABXX (WTFILE, 'READ', XXROW, TB, TE, WEIGHT, IRET)
               IF (IRET.NE.0) GO TO 999
               IF (RP(ILOCT).LT.TB) THEN
                  NSKIP = NSKIP + 1
                  GO TO 10
                  END IF
               END IF
            END IF
         IF (ILOCB.GT.0) THEN
            JANT = RP(ILOCB) + 0.001
            IANT = JANT / 256
            JANT = JANT - 256 * IANT
         ELSE
            IANT = RP(ILOCA1) + 0.1
            JANT = RP(ILOCA2) + 0.1
            END IF
C                                       Scale weights
         ONEOK = .FALSE.
         IF (DOALL.GT.0) THEN
            WT = AVG
            IF (WT.LE.0.0) THEN
               NFLAG = NPOL * NIF
            ELSE
               DO 30 LP = 1,NPOL
                  DO 20 LIF = 1,NIF
                     IP = (LP-1) * INCS + (LIF-1) * INCIF - INCF + 1
                     DO 15 LCHAN = 1,NCHAN
                        IP = IP + INCF
                        IF (VIS(3,IP).GT.0.0) ONEOK = .TRUE.
                        VIS(3,IP) = VIS(3,IP) * WT
 15                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
               END IF
C                                       Copy new weights to vis
         ELSE
            DO 60 LP = 1,NPOL
               DO 50 LIF = 1,NIF
                  IP = (LP-1) * INCS + (LIF-1) * INCIF - INCF + 1
                  WT = WEIGHT(IANT,LIF,IA(LP)) * WEIGHT(JANT,LIF,JB(LP))
                  IF (WT.LE.0) THEN
                     DO 35 LCHAN = 1,NCHAN
                        IP = IP + INCF
                        IF (VIS(3,IP).GT.0.0) THEN
                           NFLAG = NFLAG + 1
                           GO TO 50
                           END IF
 35                     CONTINUE
                  ELSE
                     DO 40 LCHAN = 1,NCHAN
                        IP = IP + INCF
                        IF (VIS(3,IP).GT.0.0) THEN
                           VIS(3,IP) = WT*((VIS(3,IP)/WTMAX)**LIMITS(5))
                           ONEOK = .TRUE.
                           END IF
 40                     CONTINUE
                     END IF
 50               CONTINUE
 60            CONTINUE
            END IF
         IF (ONEOK) THEN
            CALL UVWRIT (OUTFIL, RP, VIS, IRET)
            IF (IRET.NE.0) GO TO 999
            COUNT = COUNT + 1
         ELSE
            FLAGS = FLAGS + 1
            END IF
         GO TO 10
         END IF
C                                       Set sort order for output file:
      TYPE = OOACAR
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (OUTFIL, 'SORTORD', TYPE, DIM, IDUMMY, 'TB', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close files:
      CALL OUVCLO (IDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVCLO (OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABCLO (WTFILE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy tables:
      CALL UVCALT (IDATA, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1110) NSKIP
      IF (NSKIP.GT.0) CALL MSGWRT (6)
      WRITE (MSGTXT,1111) NFLAG
      IF (NFLAG.GT.0) CALL MSGWRT (6)
      WRITE (MSGTXT,1000) COUNT
      CALL MSGWRT (2)
      FLAGS = FLAGS + NSKIP
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I9,' visibilities written to output file')
 1110 FORMAT (I9,' visibilities omitted from output')
 1111 FORMAT (I9,' polarizations * IFs flagged on output')
      END
      SUBROUTINE FWTHIS (INPUTS, IDATA, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Update history file and display summary.
C
C   Inputs:
C      INPUTS C*(*)    Name of INPUTS object
C      IDATA   C*(*)    Name of UVDATA object for input file
C      OUTFIL C*(*)    Name of UVDATA object for output file
C
C   Output:
C      IRET   I        Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), IDATA*(*), OUTFIL*(*)
      INTEGER   IRET
C                                       Adverbs to record
      INTEGER   NADVRB
      PARAMETER (NADVRB = 13)
      CHARACTER ADVRBS(NADVRB)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FIXWT.INC'
      DATA ADVRBS /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTNAME',
     *   'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'BCHAN', 'ECHAN', 'SOLINT',
     *   'DOALL','CPARM'/
C-----------------------------------------------------------------------
      CALL OHCOPY (IDATA, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OHTIME (OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OHLIST (INPUTS, ADVRBS, NADVRB, OUTFIL, IRET)
      IF (IRET.NE.0) GO TO 999
C
      IF (FLAGS.GT.0) THEN
         WRITE (MSGTXT,1000) FLAGS
         CALL MSGWRT (5)
         CALL OHWRIT (MSGTXT, OUTFIL, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
      IF (DOALL.GT.0) THEN
         WRITE (MSGTXT,1010) AVG
         CALL OHWRIT (MSGTXT, OUTFIL, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/ Flagged', I8, ' points for undetermined weights')
 1010 FORMAT ('/ Weights multiplied by',1PE12.4)
      END
      SUBROUTINE WTSOLV (NANT, SSQ, LIMITS, WT)
C-----------------------------------------------------------------------
C   Converts array of baseline sigma**2 to antenna weights
C   Inputs:
C      NANT   I      Number antennas
C   In/Out
C      SSQ    R(*)   Sigma**2 by baseline
C   Outputs
C      WT     R(*)   Antenna weights
C-----------------------------------------------------------------------
      INTEGER   NANT
      REAL      SSQ(NANT,NANT), LIMITS(*), WT(NANT)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, NRMS, IT, NODATA(MAXANT)
      REAL      ARMS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL RFILL (NANT, 0.0, WT)
C                                       sum in Re and Im
      NRMS = 0
      ARMS = 0.0
      DO 20 I = 1,NANT
         NODATA(I) = 0
         DO 10 J = I+1,NANT
            IF (SSQ(I,J).NE.FBLANK) THEN
               IF (SSQ(J,I).NE.FBLANK) SSQ(I,J) =
     *            (SSQ(I,J) + SSQ(J,I)) / 2.0
            ELSE
               IF (SSQ(J,I).NE.FBLANK) SSQ(I,J) = SSQ(J,I)
               END IF
            IF (SSQ(I,J).LE.0.0) SSQ(I,J) = FBLANK
            IF (SSQ(I,J).NE.FBLANK) THEN
               SSQ(I,J) = MAX (LIMITS(1), MIN (LIMITS(2), SSQ(I,J)))
               NRMS = NRMS + 1
               ARMS = ARMS + 1 / SSQ(I,J)
               END IF
 10         CONTINUE
 20      CONTINUE
      IF (ARMS.LE.0.0) GO TO 900
      ARMS = NRMS / ARMS
      ARMS = SQRT (ARMS)
      WRITE (MSGTXT,1020) ARMS
      CALL MSGWRT (2)
      CALL RFILL (NANT, ARMS, WT)
C                                       iterate
      DO 90 IT = 1,5
         DO 50 I = 1,NANT
            ARMS = 0.0
            NRMS = 0
            DO 30 J = I+1,NANT
               IF (SSQ(I,J).NE.FBLANK) THEN
                  NRMS = NRMS + 1
                  ARMS = ARMS + WT(J) / SSQ(I,J)
                  END IF
 30            CONTINUE
            DO 40 J = 1,I
               IF (SSQ(J,I).NE.FBLANK) THEN
                  NRMS = NRMS + 1
                  ARMS = ARMS + WT(J) / SSQ(J,I)
                  END IF
 40            CONTINUE
            IF (ARMS.GT.0.0) THEN
               WT(I) = NRMS / ARMS
            ELSE
               NODATA(I) = 1
               END IF
 50         CONTINUE
 90      CONTINUE
      DO 100 I = 1,NANT
         IF (NODATA(I).LE.0) THEN
            IF (WT(I).GT.0.0) THEN
               WT(I) = 1.0 / WT(I)
               WT(I) = MAX (LIMITS(3), MIN (LIMITS(4), WT(I)))
               END IF
         ELSE
            WT(I) = 0.0
            END IF
 100     CONTINUE
      GO TO 999
C
 900  MSGTXT = 'WTSOLV FAILS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('WTSOLV: average rms =',1PE12.4)
      END
      SUBROUTINE OXXINI (TABLE, OPCODE, XXROW, IRET)
C-----------------------------------------------------------------------
C   Initialize a weights scratch table (type XX) for reading or writing,
C   creating it if necessary.
C
C   Inputs:
C      TABLE  C*(*)     Name of table object:  NAME, CLASS, IMSEQ, DISK
C                       and VER should be set.
C      OPCODE C*4       Operation code
C                         'READ' - open for reading
C                         'WRIT' - open for writing (create if
C                                  necessary)
C
C   Outputs:
C      XXROW  I         Next row to read or write
C      IRET   I         Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   XXROW, IRET
C                                       Table label, column labels
      CHARACTER LABEL*56, COLLAB(3)*24, COLUNT(3)*8
      INTEGER   COLTYP(3), COLDIM(3)
      INTEGER   TYPE, DIM(3), IDUMMY
      CHARACTER DUMMY
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'XXTAB.INC'
      DATA LABEL / 'WEIGHTS SCRATCH TABLE'/
      DATA COLLAB / 'TIME1', 'TIME2', 'WEIGHT' /
      DATA COLUNT / 'DAYS', 'DAYS', ' ' /
      DATA COLTYP / OOARE, OOARE, OOARE /
      DATA COLDIM / 1, 1, 1 /
C-----------------------------------------------------------------------
C                                       Fill in table type:
      TYPE = OOACAR
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'TBLTYPE', TYPE, DIM, IDUMMY, 'XX', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Initialize table data if
C                                       writing:
      IF (OPCODE.EQ.'WRIT') THEN
         TYPE = OOACAR
         DIM(1) = 56
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (TABLE, 'LABEL', TYPE, DIM, IDUMMY, LABEL, IRET)
         IF (IRET.NE.0) GO TO 999
         TYPE = OOAINT
         DIM(1) = 1
         CALL TABPUT (TABLE, 'NCOL', TYPE, DIM, 3, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABPUT (TABLE, 'VER', TYPE, DIM, 0, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         TYPE = OOACAR
         DIM(1) = 24
         DIM(2) = 3
         CALL TABPUT (TABLE, 'COLABEL', TYPE, DIM, IDUMMY, COLLAB, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 8
         CALL TABPUT (TABLE, 'COLUNIT', TYPE, DIM, IDUMMY, COLUNT, IRET)
         IF (IRET.NE.0) GO TO 999
         TYPE = OOAINT
         DIM(1) = 3
         DIM(2) = 1
         CALL TABPUT (TABLE, 'COLTYPE', TYPE, DIM, COLTYP, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         COLDIM(3) = NWEIGH
         CALL TABPUT (TABLE, 'COLDIM', TYPE, DIM, COLDIM, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Initialize table:
      CALL TABOPN (TABLE, OPCODE, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT OPEN WEIGHTS SCRATCH (XX) TABLE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get column indices
      CALL TABCOL (TABLE, 3, COLLAB, COLIDX, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get next row:
      IF (OPCODE.EQ.'READ') THEN
         XXROW = 1
      ELSE
         TYPE = OOAINT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABGET (TABLE, 'NROW', TYPE, DIM, XXROW, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         XXROW = XXROW + 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE OTABXX (TABLE, OPCODE, XXROW, TB, TE, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   Read or write a record to a weights scratch table.
C
C   Inputs:
C      TABLE   C*(*)   Name of table object
C      OPCODE  C*4     Operation code: 'READ' or 'WRIT'
C   Input/output:
C      XXROW   I       Row number to read or write (incremented on exit)
C      TB      R       Start time
C      TE      R       End time
C      WEIGHT  R(*)    Weight (Npol, Nif, Nant)
C   Output:
C      IRET    I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   XXROW, IRET
      REAL      TB, TE, WEIGHT(*)
C
      INTEGER   TYPE, DIM(3), DUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'XXTAB.INC'
C-----------------------------------------------------------------------
      IF (OPCODE.EQ.'WRIT') THEN
         TYPE = OOARE
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABDPT (TABLE, XXROW, COLIDX(1), TYPE, DIM, TB, DUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(2), TYPE, DIM, TE, DUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = NWEIGH
         CALL TABDPT (TABLE, XXROW, COLIDX(3), TYPE, DIM, WEIGHT, DUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         CALL TABDGT (TABLE, XXROW, COLIDX(1), TYPE, DIM, TB, DUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(2), TYPE, DIM, TE, DUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(3), TYPE, DIM, WEIGHT, DUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
      XXROW = XXROW + 1
C
 999  RETURN
      END

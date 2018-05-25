      SUBROUTINE FLAGUP (OPCODE, LUN, DISK, CNO, VER, BUFFER, IFGRNO,
     *   FGKOLS, FGNUMV, ID, NID, SUBA, FQID, NBASE, ANT1, ANT2, BTIME,
     *   ETIME, BIF, EIF, BCHAN, ECHAN, STOKES, REASON, NWRIT, IRET)
C-----------------------------------------------------------------------
C! Updates the Flag (FG) table.
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2005-2006, 2008, 2015
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
C   Updates the flag table (FG).  On OPCODE='FLAG' one or more entries
C   are made indicating ranges of data to be rejected.  On OPCODE='UFLG'
C   existing entries will be deselected (but not removed) if there is
C   ANY overlap in the specifications. If some data is to remain flagged
C   then those portions have to be respecified.  The purpose of
C   'UFLG' is to unflag a set of data with a specified set of criteria
C   including REASON.
C      The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Uses current values in /MAPHDR/ CATBLK and the /UVHDR/ common
C   (filled by UVPGET).
C   Inputs:
C      OPCODE   C*4      Operation desired, 'FLAG'=>make entry,
C                        'UFLG'=>deselect some part of previous entries.
C                        'REAS'=>deselect all with this REASON.
C                        'WILD'=>deselect all with this (wild-card
C                                enabled) REASON.
C                        'CLOS'=>close file.
C                        '    ' (or unrecognized) = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        FG file version
C      LUN      I        Logical unit number to use
C      ID       I(NID)   List of source ID as defined in SOURCE table
C      NID      I        Number of elements in ID
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      NBASE    I        Number of baselines described in ANT1, ANT2
C      ANT1     I(*)     First antenna number in baseline
C      ANT2     I(*)     Second antenna number in baseline
C      ETIME    R        Start time of data to be flagged (Days)
C      BTIME    R        End time of data to be flagged (Days)
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than BIF
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C      STOKES   C*4      Stokes parameter desired
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C   Input/Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(8)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      NWRIT    I        Number of FG records altered
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
c                        10 => UFLG or REAS with no parameters set
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, STOKES*4, REASON*24
      INTEGER   LUN, DISK, CNO, VER, BUFFER(*), FGKOLS(8), FGNUMV(8),
     *   ID(*), NID, SUBA, NBASE, ANT1(*), ANT2(*), BIF, EIF, BCHAN,
     *   ECHAN, FQID, NWRIT, IRET
      REAL      BTIME, ETIME
C
      CHARACTER TREAS*24
      INTEGER   FIND, ISU, IBAS, IDT, SUBT, ANTS(2), IFS(2), CHANS(2),
     *   IFGRNO, NUMREC, I4, I4REC, FQT, JT, JTRIM, RPAT(24)
      LOGICAL   PFLAGS(4), TFLAGS(4), DOREAS, LDUM, SOME
      REAL      TIMER(2), TEPS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      TEPS = 1.0 / (24.0 * 3600.0 * 10.0)
C                                       Check if 'CLOS'
      IF (OPCODE.EQ.'CLOS') GO TO 900
C                                       Check if 'REAS'
      DOREAS = (OPCODE.EQ.'REAS') .OR. (OPCODE.EQ.'WILD')
      IF (OPCODE.EQ.'WILD') CALL PSFORM (24, REASON, RPAT)
      NWRIT = 0
C                                       See if table open - check FTAB
C                                       Open file
      FIND = BUFFER(82)
      IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *   THEN
         CALL FGREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
         CALL FLGINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      IFGRNO, FGKOLS, FGNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Mark as unsorted
         BUFFER(43) = 0
         BUFFER(44) = 0
         END IF
C                                       FLAG
      IF ((OPCODE.NE.'UFLG') .AND. (.NOT.DOREAS)) THEN
C                                       Loop over baseline
         DO 120 IBAS = 1,NBASE
C                                       Get polarization flags
            CALL FLGSTK (STOKES, OPCODE, PFLAGS, LDUM, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Loop over source
            DO 110 ISU = 1,NID
               ANTS(1) = ANT1(IBAS)
               ANTS(2) = ANT2(IBAS)
               TIMER(1) = BTIME
               TIMER(2) = ETIME
               IFS(1) = MAX (0, BIF)
               IFS(2) = MAX (0, EIF)
               CHANS(1) = MAX (0, BCHAN)
               CHANS(2) = MAX (0, ECHAN)
C                                       Flag table entry.
               CALL TABFLG ('WRIT', BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *            ID(ISU), SUBA, FQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IRET)
               IF (IRET.NE.0) GO TO 999
               NWRIT = NWRIT + 1
 110           CONTINUE
 120        CONTINUE
C                                       UNFLAG
      ELSE
C                                       did caller specify something
         SOME = (REASON.NE.' ') .OR. (SUBA.GT.0) .OR. (FQID.GT.0) .OR.
     *      (BTIME.GT.0.0) .OR. (ETIME.GT.0.0) .OR. (BIF.GT.0) .OR.
     *      (EIF.GT.0) .OR. (BCHAN.GT.0) .OR. (ECHAN.GT.0)
         IF (.NOT.SOME) THEN
            DO 210 IBAS = 1,NBASE
               SOME = SOME .OR. (ANT1(IBAS).GT.0) .OR. (ANT2(IBAS).GT.0)
 210           CONTINUE
            DO 215 ISU = 1,NID
               SOME = SOME .OR. (ID(ISU).GT.0)
 215           CONTINUE
            END IF
         IF ((DOREAS) .AND. (REASON.EQ.' ')) SOME = .FALSE.
         IF (.NOT.SOME) THEN
            MSGTXT = 'FLAGUP CALLED TO UNFLAG WITH NO PARMS SET'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 999
            END IF
C                                       Find number of entries in table
         NUMREC = BUFFER(5)
C                                       Search table.
         DO 290 I4REC = 1,NUMREC
            I4 = I4REC
            CALL TABFLG ('READ', BUFFER, I4, FGKOLS, FGNUMV, IDT, SUBT,
     *         FQT, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
            IF (IRET.LT.0) GO TO 290
            IF (IRET.NE.0) GO TO 999
c                                       Make sure blank filled
            JT = JTRIM (TREAS)
            IF (OPCODE.EQ.'REAS') THEN
               IF (TREAS.EQ.REASON) GO TO 250
               GO TO 290
            ELSE IF (OPCODE.EQ.'WILD') THEN
               CALL CHWMAT (24, REASON, RPAT, 1, TREAS, LDUM)
               IF (LDUM) GO TO 250
               GO TO 290
               END IF
C                                       Check invariants
C                                       Subarray
            IF ((SUBT.NE.SUBA) .AND. (SUBA.GE.0)) GO TO 290
C                                       Freq. ID
            IF (FQT.NE.-1) THEN
               IF ((FQT.NE.FQID) .AND. (FQT.GT.0) .AND.
     *            (FQID.GT.0)) GO TO 290
               END IF
C                                       Timerange
            IF ((BTIME.GE.0.0) .AND. (ABS(TIMER(1)-BTIME).GT.TEPS))
     *         GO TO 290
            IF ((ETIME.GE.0.0) .AND. (ABS(TIMER(2)-ETIME).GT.TEPS))
     *         GO TO 290
C                                       IFs
            IF ((BIF.GE.0) .AND. (IFS(1).NE.BIF)) GO TO 290
            IF ((EIF.GE.0) .AND. (IFS(2).NE.EIF)) GO TO 290
C                                       Channels
            IF ((BCHAN.GE.0) .AND. (CHANS(1).NE.BCHAN)) GO TO 290
            IF ((ECHAN.GE.0) .AND. (CHANS(2).NE.ECHAN)) GO TO 290
C                                       REASON
            IF ((REASON.NE.TREAS) .AND. (REASON.NE.' ')) GO TO 290
C                                       Loop over source
            IF (ID(1).GE.0) THEN
               DO 230 ISU = 1,NID
                  IF (IDT.EQ.ID(ISU)) GO TO 235
 230              CONTINUE
               IF ((IDT.GT.0) .OR. (ID(1).GT.0)) GO TO 290
               END IF
C                                       Loop over baseline
 235        DO 240 IBAS = 1,NBASE
C                                       Test baseline dependencies
C                                       Antenna 1
               IF ((ANTS(1).NE.ANT1(IBAS)) .AND. (ANT1(IBAS).GT.0))
     *            GO TO 240
C                                       Antenna 2
               IF ((ANTS(2).NE.ANT2(IBAS)) .AND. (ANT2(IBAS).GT.0))
     *            GO TO 240
C                                       Get polarization flags
               IF (STOKES.EQ.'----') THEN
                  GO TO 250
               ELSE
                  CALL FLGSTK (STOKES, OPCODE, PFLAGS, LDUM, IRET)
C                                       PFLAGS
                  IF ((IRET.EQ.0) .AND. (PFLAGS(1).EQV.TFLAGS(1)) .AND.
     *               (PFLAGS(2).EQV.TFLAGS(2)) .AND.
     *               (PFLAGS(3).EQV.TFLAGS(3)) .AND.
     *               (PFLAGS(4).EQV.TFLAGS(4))) GO TO 250
                  END IF
 240           CONTINUE
C                                       No match
            GO TO 290
C                                       Deselect entry.
 250        I4 = I4REC
            CALL TABFLG ('FLAG', BUFFER, I4, FGKOLS, FGNUMV, IDT, SUBT,
     *         FQT, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
            IF (IRET.NE.0) GO TO 999
            NWRIT = NWRIT + 1
 290        CONTINUE
         END IF
      GO TO 999
C                                       Close
 900  CALL TABFLG ('CLOS', BUFFER, IFGRNO, FGKOLS, FGNUMV, IDT, SUBA,
     *   FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C
 999  RETURN
      END

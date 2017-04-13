      SUBROUTINE SNINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize solution (SN) tables.
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006, 2015
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
C   Creates and initializes solution (SN) extension tables.
C   REVISION 10
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   SN file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C   Input (create) / output (pre-existing)
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of IFs per group
C     NUMIF        I    Number of IF groups
C     NUMNOD       I    Number of interpolation nodes. Will handle
C                       up to 25 interpolation nodes.
C     GMMOD        R    Mean gain modulus
C     RANOD(*)     R    RA offset of interpolation nodes (deg.)
C     DECNOD(*)    R    Dec. offset of interpolation nodes (deg.)
C     ISAPPL       L    True if this SN table has been applied to
C                       the  CL table.
C   Output:
C     ISNRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     SNKOLS(MAXSNC) I   The column pointer array in order defined by
C                        the parameters given in PSNTAB.INC.
C     SNNUMV(MAXSNC) I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ISNRNO,
     *   SNKOLS(*), SNNUMV(*), NUMANT, NUMPOL, NUMIF, NUMNOD, IERR
      REAL      GMMOD, RANOD(25), DECNOD(25)
      LOGICAL   ISAPPL
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      CHARACTER KEYW(57)*8, TTITLE*56, TITLE(MAXSNC)*24, UNITS(MAXSNC)*8
      HOLLERITH HOLTMP(6)
      INTEGER   NKEY, NREC, DATP(128,2), NCOL
      REAL      KEYVAR(120)
      INTEGER   NTT, DTYP(MAXSNC), NDATA, KLOCS(57), KEYVAL(120), NC,
     *   KEYTYP(57), IPOINT, J, JNUM, I, JERR, REVNO, MSGSAV, ITRIM
      DOUBLE PRECISION KEYVAD
      LOGICAL   T, DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYVAL, KEYVAR)
      DATA T /.TRUE./
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE SOLUTION TABLE '/
C                                        Values larger than 10 are
C                                        scalars
      DATA DTYP /11,12,14,14,14,14,12,14,3*12,5*2,4,3*12,5*2,4/
C                                       NOTE: change PSNTAB.INC if the
C                                       columns are changed.
      DATA TITLE /'TIME                    ',
     *   'TIME INTERVAL          ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ', 'NODE NO.                ',
     *   'MBDELAY1                ',
     *   'DISP 1                  ', 'DDISP 1                 ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'DELAY 1                 ', 'RATE 1                  ',
     *   'WEIGHT 1                ', 'REFANT 1                ',
     *   'MBDELAY2                ',
     *   'DISP 2                  ', 'DDISP 2                 ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'DELAY 2                 ', 'RATE 2                  ',
     *   'WEIGHT 2                ', 'REFANT 2                '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ',
     *   'NO_NODES', 'MGMOD   ', 'APPLIED ', 'REVISION',
     *   'RA_OFF1 ', 'DEC_OFF1', 'RA_OFF2 ', 'DEC_OFF2',
     *   'RA_OFF3 ', 'DEC_OFF3', 'RA_OFF4 ', 'DEC_OFF4',
     *   'RA_OFF5 ', 'DEC_OFF5', 'RA_OFF6 ', 'DEC_OFF6',
     *   'RA_OFF7 ', 'DEC_OFF7', 'RA_OFF8 ', 'DEC_OFF8',
     *   'RA_OFF9 ', 'DEC_OFF9', 'RA_OFF10', 'DEC_OF10',
     *   'RA_OFF11', 'DEC_OF11', 'RA_OFF12', 'DEC_OF12',
     *   'RA_OFF13', 'DEC_OF13', 'RA_OFF14', 'DEC_OF14',
     *   'RA_OFF15', 'DEC_OF15', 'RA_OFF16', 'DEC_OF16',
     *   'RA_OFF17', 'DEC_OF17', 'RA_OFF18', 'DEC_OF18',
     *   'RA_OFF19', 'DEC_OF19', 'RA_OFF20', 'DEC_OF20',
     *   'RA_OFF21', 'DEC_OF21', 'RA_OFF22', 'DEC_OF22',
     *   'RA_OFF23', 'DEC_OF23', 'RA_OFF24', 'DEC_OF24',
     *   'RA_OFF25', 'DEC_OF25'/
      DATA UNITS /'DAYS', 'DAYS', 4*' ', 'RAD/M**2',
     *    ' ', 'SECONDS', 'SEC/M**2', 'S/S/M**2', 2*' ', 'SECONDS ',
     *    'SEC/SEC', 2*' ', 'SECONDS', 'SEC/M**2', 'S/S/M**2',
     *    2*' ', 'SECONDS ', 'SEC/SEC', 2*' '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 8 + NUMPOL * 9
      IF (DOREAD) NCOL = 0
      NKEY = 7 + NUMNOD * 2
      NDATA = MAXSNC
      CALL FILL (NDATA, 0, SNKOLS)
      CALL FILL (NDATA, 0, SNNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 1,NDATA
            IF (DTYP(J).LT.10) DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'SN', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SNINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of scans
      ISNRNO = BUFFER(5) + 1
      IF (DOREAD) ISNRNO = 1
      NKEY = 7 + NUMNOD * 2
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SNINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SNINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       No. interpolation nodes.
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = NUMNOD
C                                       Gain modulus
         KLOCS(5) = 5
         KEYTYP(5) = 1
         KEYVAD = GMMOD
         IPOINT = KLOCS(5)
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       APPLIED flag
         KLOCS(6) = IPOINT
         KEYTYP(6) = 5
         CALL COPY (1, ISAPPL, KEYVAL(IPOINT))
         IPOINT = IPOINT + 1
C                                       Revision number from PSNTAB.INC
         REVNO = SNREV
         KLOCS(7) = IPOINT
         KEYTYP(7) = 4
         KEYVAL(IPOINT) = REVNO
         IPOINT = IPOINT + 1
C                                       Interpolation nodes
         IF (NUMNOD.GT.0) THEN
            JNUM = 8
            DO 50 J = 1,NUMNOD
               KLOCS(JNUM) = IPOINT
               KEYTYP(JNUM) = 1
               KEYVAD = RANOD(J)
               CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
               IPOINT = IPOINT + NWDPDP
               JNUM = JNUM + 1
               KLOCS(JNUM) = IPOINT
               KEYTYP(JNUM) = 1
               KEYVAD = DECNOD(J)
               CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
               IPOINT = IPOINT + NWDPDP
               JNUM = JNUM + 1
 50            CONTINUE
            END IF
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'SNINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'SNINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per group.
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF groups.
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. interpolation nodes.
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) NUMNOD = KEYVAL(IPOINT)
C                                       Gain modulus
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(5).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
               GMMOD = KEYVAD
            ELSE
               GMMOD = KEYVAR(IPOINT)
               END IF
            END IF
C                                       APPLIED flag
         IPOINT = KLOCS(6)
         IF (IPOINT.GT.0) CALL COPY (1, KEYVAL(IPOINT), ISAPPL)
C                                       Revision level
         IPOINT = KLOCS(7)
         REVNO = -1
         IF (IPOINT.GT.0) REVNO = KEYVAL(IPOINT)
C                                       Interpolation nodes
         IF (NUMNOD.GT.0) THEN
            JNUM = 8
            DO 100 J = 1,NUMNOD
               IPOINT = KLOCS(JNUM)
               IF (IPOINT.GT.0) THEN
                  IF (KEYTYP(JNUM).EQ.1) THEN
                     CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
                     RANOD(J) = KEYVAD
                  ELSE
                     RANOD(J) = KEYVAR(IPOINT)
                     END IF
                  END IF
               JNUM = JNUM + 1
               IPOINT = KLOCS(JNUM)
               IF (IPOINT.GT.0) THEN
                  IF (KEYTYP(JNUM).EQ.1) THEN
                     CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
                     DECNOD(J) = KEYVAD
                  ELSE
                     DECNOD(J) = KEYVAR(IPOINT)
                     END IF
                  END IF
               JNUM = JNUM + 1
 100           CONTINUE
            END IF
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, ISNRNO, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SNINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'SN', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SNINI', IERR)
         GO TO 990
         END IF
      NCOL = 8 + NUMPOL * 9
      CALL FNDCOL (NCOL, TITLE, 24, T, BUFFER, SNKOLS, JERR)
C                                       no message on DISP, DDISP
      DO 150 I = 1,NCOL
         IPOINT = SNKOLS(I)
         IF (IPOINT.GT.0) THEN
            SNKOLS(I) = DATP(IPOINT,1)
            SNNUMV(I) = DATP(IPOINT,2) / 10
            IF (SNNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF ((I.NE.10) .AND. (I.NE.11) .AND. (I.NE.19). AND.
     *            (I.NE.20)) CALL MSGWRT (6)
               END IF
         ELSE
            SNKOLS(I) = -1
            SNNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF ((I.NE.10) .AND. (I.NE.11) .AND. (I.NE.19). AND.
     *         (I.NE.20)) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('SNINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('SNINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('SNINI: ERROR INITIALIZING SOLUTION TABLE FOR ',A4)
      END

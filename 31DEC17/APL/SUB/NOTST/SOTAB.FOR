      SUBROUTINE SOTAB (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! I/O to source tables (following initialization by SOINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2010, 2012
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
C   Does I/O to source (SO) tables. Usually used after setup by
C   SOINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          SOINI.
C   Inputs via common:
C      ISORNO   I          Next scan number to read or write.
C      SOKOLS   I(MAXSOC)  The column pointer array in order:
C                          ID. SO., SOURCE, QUAL, CALCODE, FREQID,
C                          IFLUX, QFLUX, UFLUX, VFLUX, ALPHA, FREQOFF,
C                          RAEPO, DECEPO, EQUINX, RAAPP, DECAPP, SYSVEL,
C                          VELTYP, VELDEF, RESTFRQ, PMRA, PMDEC,
C                          PARALLAX
C      SONUMV   I(MAXSOC)  Element count in each column.
C
C   Input/output: (written to or read from source file)
C      ISOUR    I          Source ID number.
C      SAUCE    C*16       Source name.
C      QUAL     I          Source qualifier.
C      CALCOD   C*4        Calibrator code.
C      FREQID   I          Frequency ID number.
C      IFLUX    R(MAXIF)   I flux density at reference frequency.
C      QFLUX    R(MAXIF)   Q flux density at reference frequency.
C      UFLUX    R(MAXIF)   U flux density at reference frequency.
C      VFLUX    R(MAXIF)   V flux density at reference frequency.
C      ALPHA    R(MAXIF)     Spectral index of source / band
C                            defined as: S = So * FREQ ** -ALPHA
C      FRQOFF   D(MAXIF)   Frequency offset from reference PLUS
C                            frequency table offset / band.
C      RAEPO    D          RA at standard neam epoch.
C      DECEPO   D          Dec at standard mean epoch.
C      EQUINX    D          Standard epoch.
C      RAAPP    D          Apparent RA at 0h IAT on reference day in
C                            UV header.
C      DECAPP   D          Apparent Dec at 0h IAT on reference dat in
C                            UV header.
C      SYSVEL   D(MAXIF)   Systemic velocity at the reference pixel
C                            (channel) of each band. Reference pixel
C                            is defined by CRPIXn in UV binary table.
C      VELTYP   C*8        Velocity type --- 'LSR',' BARYCENT',
C                            'GEOCNTR', 'TOPOCENT'.
C      VELDEF   C*8        Velocity definition --- 'RADIO' or
C                            'OPTICAL'.
C      RSTFRQ   D(MAXIF)   Line rest frequency.
C      PMRA     D          Proper motion in RA.
C      PMDEC    D          Proper motion in Dec.
C      PARLAX   R          Parallax of source (if measurable).
C
C   Output:
C      ISORNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DSOV.INC' for the declarations in
C   SOINI and SOTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   NCOPY, I
      CHARACTER EQNX*8
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOV.INC'
      INTEGER   RECI(MAXREC)
      REAL      RECORD(MAXREC)
      HOLLERITH RECH(MAXREC)
      DOUBLE PRECISION RECD(MAXRCD)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Source id
         RECI(SOKOLS(KSID)) = ISOUR
C                                       Source name
         CALL CHR2H (16, SAUCE, 1, RECH(SOKOLS(KNAM)))
C                                       Qualifier
         RECI(SOKOLS(KQUA)) = QUAL
C                                       Calibrator code
         CALL CHR2H(4, CALCOD, 1, RECH(SOKOLS(KCOD)))
C                                       Frequency id
         RECI(SOKOLS(KFRQ)) = FREQID
C                                       I flux density
         NCOPY = SONUMV(KIFX)
         CALL RCOPY (NCOPY, IFLUX, RECORD(SOKOLS(KIFX)))
C                                       Q flux density
         NCOPY = SONUMV(KQFX)
         CALL RCOPY (NCOPY, QFLUX, RECORD(SOKOLS(KQFX)))
C                                       U flux density
         NCOPY = SONUMV(KUFX)
         CALL RCOPY (NCOPY, UFLUX, RECORD(SOKOLS(KUFX)))
C                                       V flux density
         NCOPY = SONUMV(KVFX)
         CALL RCOPY (NCOPY, VFLUX, RECORD(SOKOLS(KVFX)))
C                                       Spectral index
         NCOPY = SONUMV(KALF)
         CALL RCOPY (NCOPY, ALPHA, RECORD(SOKOLS(KALF)))
C                                       Frequency offset
         DO 100 I = 1, SONUMV(KFQO)
            RECD(SOKOLS(KFQO)+I-1) = FRQOFF(I)
 100        CONTINUE
C                                       RA of reference epoch
         RECD(SOKOLS(KRAE)) = RAEPO
C                                       Dec of reference epoch
         RECD(SOKOLS(KDEE)) = DECEPO
C                                       Epoch. VLBA incorrectly
C                                       writes this as a double
C                                       and we check for this
C                                       using the column dimension
         IF (SONUMV(KEQU) .EQ. 8) THEN
C                                       Only B1950.0 and J2000
C                                       are allowed. This section
C                                       is not critical since we
C                                       do not normally write SO
C                                       tables.
            IF (ABS(EQUINX-1950.0).LT.0.1) THEN
               CALL CHR2H (8, '1950.0B ', 1, RECH(SOKOLS(KEQU)))
            ELSE
               CALL CHR2H (8, 'J2000   ', 1, RECH(SOKOLS(KEQU)))
            END IF
         ELSE
            RECD(SOKOLS(KEQU)) = EQUINX
         END IF
C                                       RA of reference date
         RECD(SOKOLS(KRAA)) = RAAPP
C                                       Dec of reference date
         RECD(SOKOLS(KDEA)) = DECAPP
C                                       RA of reference date
         RECD(SOKOLS(KRAO)) = RAOBS
C                                       Dec of reference date
         RECD(SOKOLS(KDEO)) = DECOBS
C                                       Systemic velocity
         DO 110 I = 1, SONUMV(KSVL)
            RECD(SOKOLS(KSVL)+I-1) = SYSVEL(I)
 110        CONTINUE
C                                       Velocity type
         CALL CHR2H (8, VELTYP, 1, RECH(SOKOLS(KVTP)))
C                                       Velocity definition
         CALL CHR2H (8, VELDEF, 1, RECH(SOKOLS(KVDF)))
C                                       Line rest frequency
         DO 120 I = 1, SONUMV(KRFQ)
            RECD(SOKOLS(KRFQ)+I-1) = RSTFRQ(I)
 120        CONTINUE
C                                       Proper motion --- RA
         RECD(SOKOLS(KPMR)) = PMRA
C                                       Proper motion --- Dec
         RECD(SOKOLS(KPMD)) = PMDEC
C                                       Parallax
         RECORD(SOKOLS(KPAR)) = PARLAX
C
         ENDIF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ISORNO, RECORD, BUFFER, IERR)
      ISORNO = ISORNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'SOTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Source id
         ISOUR = RECI(SOKOLS(KSID))
C                                       Source name
         CALL H2CHR (16, 1, RECH(SOKOLS(KNAM)), SAUCE)
C                                       Qualifier
         QUAL = RECI(SOKOLS(KQUA))
C                                       Calibrator code
         CALL H2CHR (4, 1, RECH(SOKOLS(KCOD)), CALCOD)
C                                       Frequency ID
         FREQID = RECI(SOKOLS(KFRQ))
C                                       I flux density
         NCOPY = SONUMV(KIFX)
         CALL RCOPY (NCOPY, RECORD(SOKOLS(KIFX)), IFLUX)
C                                       Q flux density
         NCOPY = SONUMV(KQFX)
         CALL RCOPY (NCOPY, RECORD(SOKOLS(KQFX)), QFLUX)
C                                       U flux density
         NCOPY = SONUMV(KUFX)
         CALL RCOPY (NCOPY, RECORD(SOKOLS(KUFX)), UFLUX)
C                                       V flux density
         NCOPY = SONUMV(KVFX)
         CALL RCOPY (NCOPY, RECORD(SOKOLS(KVFX)), VFLUX)
C                                       Spectral index
         NCOPY = SONUMV(KALF)
         CALL RCOPY (NCOPY, RECORD(SOKOLS(KALF)), ALPHA)
C                                       Frequency offset
         DO 200 I = 1, SONUMV(KFQO)
            FRQOFF(I) = RECD(SOKOLS(KFQO)+I-1)
 200        CONTINUE
C                                       RA of reference epoch
         RAEPO = RECD(SOKOLS(KRAE))
C                                       Dec of reference epoch
         DECEPO = RECD(SOKOLS(KDEE))
C                                       Epoch. The VLBA incorrectly
C                                       uses double precision rather
C                                       than a string so we check
C                                       the column dimension to see
C                                       what we have
         IF (SONUMV(KEQU) .EQ. 8) THEN
            CALL H2CHR (8, 1, RECH(SOKOLS(KEQU)), EQNX)
            IF (EQNX.EQ.'B1950.0 ') THEN
               EQUINX = 1950.0D0
            ELSE IF (EQNX.EQ.'1950.0B ') THEN
               EQUINX = 1950.0D0
            ELSE IF (EQNX.EQ.'J2000   ') THEN
               EQUINX = 2000.0D0
C                                       No other strings allowed so
C                                       give an obvious bad epoch
            ELSE
               EQUINX = -1.0D0
               WRITE (MSGTXT, 9020) EQNX
               CALL MSGWRT (5)
               END IF
         ELSE
            EQUINX = RECD(SOKOLS(KEQU))
            END IF
C                                       RA of reference date
         RAAPP = RECD(SOKOLS(KRAA))
C                                       Dec of reference date
         DECAPP = RECD(SOKOLS(KDEA))
C                                       RA of pointing
         IF (SONUMV(KRAO).GT.0) THEN
            RAOBS = RECD(SOKOLS(KRAO))
         ELSE
            RAOBS = RECD(SOKOLS(KRAE))
            END IF
C                                       Dec of pointing
         IF (SONUMV(KDEO).GT.0) THEN
            DECOBS = RECD(SOKOLS(KDEO))
         ELSE
            DECOBS = RECD(SOKOLS(KDEE))
            END IF
C                                       Systemic velocity
         DO 210 I = 1, SONUMV(KSVL)
            SYSVEL(I) = RECD(SOKOLS(KSVL)+I-1)
 210        CONTINUE
C                                       Velocity type
         CALL H2CHR (8, 1, RECH(SOKOLS(KVTP)), VELTYP)
C                                       Velocity definition
         CALL H2CHR (8, 1, RECH(SOKOLS(KVDF)), VELDEF)
C                                       Rest frequency
         DO 220 I = 1, SONUMV(KRFQ)
            RSTFRQ(I) = RECD(SOKOLS(KRFQ)+I-1)
 220        CONTINUE
C                                       Proper motion --- RA
         PMRA = RECD(SOKOLS(KPMR))
C                                       Proper motion --- Dec
         PMDEC = RECD(SOKOLS(KPMD))
C                                       Parallax
         PARLAX = RECORD(SOKOLS(KPAR))
C
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ISORNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SOTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('SOTAB: TABIO ERROR',I5)
 9020 FORMAT ('SOTAB: ', A8, ' IS NOT A VALID EQUINOX')
      END

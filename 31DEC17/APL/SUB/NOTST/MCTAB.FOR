      SUBROUTINE MCTAB (OPCODE, BUFFER, IMCRNO, MCKOLS, MCNUMV, NUMPOL,
     *   NUMIF, TIME, SOURID, ANTNO, SUBA, FREQID, ATMOS, DATMOS,
     *   GDELAY, GRATE, CLOCK, DCLOCK, LOOFF, DLOOFF, DISP, DDISP, IERR)
C-----------------------------------------------------------------------
C! Does I/O to Model Components (MC) table opened by MCINI
C# EXT-util UV Calibration VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2009, 2011
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
C   Does I/O to Model Components table.
C   Inputs:
C      OPCODE   C*4       Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        TABINI or MCINI.
C      IMCRNO   I        Next scan number to read or write.
C      MCKOLS   I(21)    The column pointer array in order defined by
C                        the parameters in MCINI
C      MCNUMV   I(21)    Element count in each column.
C      NUMPOL   I        Number of polarizations per IF.
C      NUMIF    I        Number of IFs.
C   Input/output: (written to or read from CAL file)
C      TIME     D        Start time of MC record (Days)
C      SOURID   I        Source ID as defined in the SoUrce table.
C      ANTNO    I        Antenna number as defined in Antenna table
C      SUBA     I        Subarray number.
C      FREQID   I        Freqid # as defined in FQ table.
C      ATMOS    D        Atmospheric delay (sec) 1/Poln
C      DATMOS   D        Time derivative of ATMOS (sec/sec)
C      GDELAY   D        Group delay (sec)
C      GRATE    D        Rate of change of GDELAY (sec/sec)
C      CLOCK    D(2)     "Clock" epoch error (sec) 1/poln
C      DCLOCK   D(2)     Time derivative of CLOCK (sec/sec)
C      LOOFF    R(2,*)   LO offset (pol,IF) in Hz
C      DLOOFF   R(2,*)   Change in LOOFF (pol,IF) in Hz/sec
C      DISP     R(2,*)   Dispersive delay (sec/m^2 scales as lambda^2)
C                           1/poln
C      DDISP    R(2,*)   Time derivative of DISP (sec/sec/m^2)
C   Output:
C      IMCRNO   I        Next MC number.
C      IERR     I        Error code, 0=>OK else TABIO error.
C                           Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      DOUBLE PRECISION TIME, ATMOS, DATMOS, GDELAY, GRATE, CLOCK(2),
     *   DCLOCK(2)
      REAL      LOOFF(2,MAXIF), DLOOFF(2,MAXIF), DISP(2), DDISP(2)
      INTEGER   BUFFER(512), MCKOLS(21), MCNUMV(21), IMCRNO, NUMPOL,
     *   NUMIF, SOURID, ANTNO, SUBA, FREQID, IERR
C                                       Parameters of MC table
C                                       3rd character gives data type:
C                                       D=double, R=real, I=integer
      INTEGER   MCDTIM, MCISID, MCIANT, MCISUB, MCIFQI, MCDATM, MCDDAT,
     *   MCDDEL, MCDRAT, MCDCK1, MCDDC1, MCRLO1, MCRDL1, MCRDS1, MCRDD1,
     *   MCDCK2, MCDDC2, MCRLO2, MCRDL2, MCRDS2, MCRDD2, XMCRSZ
C                                       XMCRSZ
      PARAMETER (XMCRSZ = 8 + 2 * 9 + 4 * MAXIF)
C                                       MCDTIM = Time
      PARAMETER (MCDTIM = 1)
C                                       MCISID = Source ID.
      PARAMETER (MCISID = MCDTIM+1)
C                                       MCIANT = Antenna number
      PARAMETER (MCIANT = MCISID+1)
C                                       MCISUB = Subarray number
      PARAMETER (MCISUB = MCIANT+1)
C                                       MCIFQI = Freq. ID.
      PARAMETER (MCIFQI = MCISUB+1)
C                                       MCDATM = Atmos. delay
      PARAMETER (MCDATM = MCIFQI+1)
C                                       MCDDAT = Time deriv. MCDATM
      PARAMETER (MCDDAT = MCDATM+1)
C                                       MCDDEL = Delay
      PARAMETER (MCDDEL = MCDDAT+1)
C                                       MCDRAT = RATE
      PARAMETER (MCDRAT = MCDDEL+1)
C                                       MCDCK1 = Clock delay Stokes 1
      PARAMETER (MCDCK1 = MCDRAT+1)
C                                       MCDDC1 = Time deriv. MCDCK1
      PARAMETER (MCDDC1 = MCDCK1+1)
C                                       MCRLO1 = LO offset pol 1
      PARAMETER (MCRLO1 = MCDDC1 + 1)
C                                       MCRLO1 = LO offset change pol 1
      PARAMETER (MCRDL1 = MCRLO1 + 1)
C                                       MCRDS1 = Dispersive delay 1
      PARAMETER (MCRDS1 = MCRDL1+1)
C                                       MCRDD1 = Time deriv. MCRDS1
      PARAMETER (MCRDD1 = MCRDS1+1)
C                                       MCDCK2 = Clock delay Stokes 2
      PARAMETER (MCDCK2 = MCRDD1+1)
C                                       MCDDC2 = Time deriv. MCDCK2
      PARAMETER (MCDDC2 = MCDCK2+1)
C                                       MCRLO1 = LO offset pol 2
      PARAMETER (MCRLO2 = MCDDC2 + 1)
C                                       MCRLO1 = LO offset change pol 2
      PARAMETER (MCRDL2 = MCRLO2 + 1)
C                                       MCRDS2 = Dispersive delay 2
      PARAMETER (MCRDS2 = MCRDL2+1)
C                                       MCRDD2 = Time deriv. MCRDS2
      PARAMETER (MCRDD2 = MCRDS2+1)
C                                       local variables
      INTEGER   RECI(XMCRSZ), LOOP, LOOKOL, DLOKOL
      REAL      RECR(XMCRSZ)
      DOUBLE PRECISION RECD(XMCRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECR, RECI)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IMCRNO, RECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 990
         GO TO 999
         END IF
C                                       If write fill RECR
      IF (OPCODE.EQ.'WRIT') THEN
         CALL RFILL (XMCRSZ, 0.0, RECR)
         RECD(MCKOLS(MCDTIM)) = TIME
         RECI(MCKOLS(MCISID)) = SOURID
         RECI(MCKOLS(MCIANT)) = ANTNO
         RECI(MCKOLS(MCISUB)) = SUBA
         IF (MCKOLS(MCIFQI).GT.0) RECI(MCKOLS(MCIFQI)) = FREQID
         IF (MCKOLS(MCDATM).GT.0) RECD(MCKOLS(MCDATM)) = ATMOS
         IF (MCKOLS(MCDDAT).GT.0) RECD(MCKOLS(MCDDAT)) = DATMOS
         IF (MCKOLS(MCDDEL).GT.0) RECD(MCKOLS(MCDDEL)) = GDELAY
         IF (MCKOLS(MCDRAT).GT.0) RECD(MCKOLS(MCDRAT)) = GRATE
         IF (MCKOLS(MCDCK1).GT.0) RECD(MCKOLS(MCDCK1)) = CLOCK(1)
         IF (MCKOLS(MCDDC1).GT.0) RECD(MCKOLS(MCDDC1)) = DCLOCK(1)
         IF (MCKOLS(MCRDS1).GT.0) RECR(MCKOLS(MCRDS1)) = DISP(1)
         IF (MCKOLS(MCRDD1).GT.0) RECR(MCKOLS(MCRDD1)) = DDISP(1)
C                                       Setup for looping over IF
         LOOKOL = MCKOLS(MCRLO1)
         DLOKOL = MCKOLS(MCRDL1)
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECR(LOOKOL+LOOP-1) = LOOFF(1,LOOP)
            RECR(DLOKOL+LOOP-1) = DLOOFF(1,LOOP)
 10         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            IF (MCKOLS(MCDCK2).GT.0) RECD(MCKOLS(MCDCK2)) = CLOCK(2)
            IF (MCKOLS(MCDDC2).GT.0) RECD(MCKOLS(MCDDC2)) = DCLOCK(2)
            IF (MCKOLS(MCRDS2).GT.0) RECR(MCKOLS(MCRDS2)) = DISP(2)
            IF (MCKOLS(MCRDD2).GT.0) RECR(MCKOLS(MCRDD2)) = DDISP(2)
C                                       Setup for looping over IF
            LOOKOL = MCKOLS(MCRLO2)
            DLOKOL = MCKOLS(MCRDL2)
            DO 20 LOOP = 1,NUMIF
               RECR(LOOKOL+LOOP-1) = LOOFF(2,LOOP)
               RECR(DLOKOL+LOOP-1) = DLOOFF(2,LOOP)
 20            CONTINUE
            END IF
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IMCRNO, RECR, BUFFER, IERR)
      IMCRNO = IMCRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         TIME = RECD(MCKOLS(MCDTIM))
         SOURID = RECI(MCKOLS(MCISID))
         ANTNO = RECI(MCKOLS(MCIANT))
         SUBA = RECI(MCKOLS(MCISUB))
         FREQID = 1
         ATMOS = 0.0D0
         DATMOS = 0.0D0
         GDELAY = 0.0D0
         GRATE = 0.0D0
         CLOCK(1) = 0.0D0
         DCLOCK(1) = 0.0D0
         DISP(1) = 0.0
         DDISP(1) = 0.0
         CLOCK(2) = 0.0D0
         DCLOCK(2) = 0.0D0
         DISP(2) = 0.0
         DDISP(2) = 0.0
         IF (MCKOLS(MCIFQI).GT.0) FREQID = RECI(MCKOLS(MCIFQI))
         IF (MCKOLS(MCDATM).GT.0) ATMOS = RECD(MCKOLS(MCDATM))
         IF (MCKOLS(MCDDAT).GT.0) DATMOS = RECD(MCKOLS(MCDDAT))
         IF (MCKOLS(MCDDEL).GT.0) GDELAY = RECD(MCKOLS(MCDDEL))
         IF (MCKOLS(MCDRAT).GT.0) GRATE = RECD(MCKOLS(MCDRAT))
         IF (MCKOLS(MCDCK1).GT.0) CLOCK(1) = RECD(MCKOLS(MCDCK1))
         IF (MCKOLS(MCDDC1).GT.0) DCLOCK(1) = RECD(MCKOLS(MCDDC1))
         IF (MCKOLS(MCRDS1).GT.0) DISP(1) = RECR(MCKOLS(MCRDS1))
         IF (MCKOLS(MCRDD1).GT.0) DDISP(1) = RECR(MCKOLS(MCRDD1))
C                                       Setup for looping over IF
         LOOKOL = MCKOLS(MCRLO1)
         DLOKOL = MCKOLS(MCRDL1)
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            LOOFF(1,LOOP) = RECR(LOOKOL+LOOP-1)
            DLOOFF(1,LOOP) = RECR(DLOKOL+LOOP-1)
 60         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            IF (MCKOLS(MCDCK2).GT.0) CLOCK(2) = RECD(MCKOLS(MCDCK2))
            IF (MCKOLS(MCDDC2).GT.0) DCLOCK(2) = RECD(MCKOLS(MCDDC2))
            IF (MCKOLS(MCRDS2).GT.0) DISP(2) = RECR(MCKOLS(MCRDS2))
            IF (MCKOLS(MCRDD2).GT.0) DDISP(2) = RECR(MCKOLS(MCRDD2))
C                                       Setup for looping over IF
            LOOKOL = MCKOLS(MCRLO2)
            DLOKOL = MCKOLS(MCRDL2)
            DO 70 LOOP = 1,NUMIF
               LOOFF(2,LOOP) = RECR(LOOKOL+LOOP-1)
               DLOOFF(2,LOOP) = RECR(DLOKOL+LOOP-1)
 70            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('MCTAB: TABIO ERROR',I3)
      END

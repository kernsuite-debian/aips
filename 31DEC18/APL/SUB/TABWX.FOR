      SUBROUTINE TABWX (OPCODE, WXBUFF, IWXRNO, WXKOLS, WXNUMV, TIME,
     *   DTIME, IANT, SUBA, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST,
     *   PRECIP, H2OCOL, IONCOL, IERR)
C----------------------------------------------------------------------
C! I/O to weather info table (following init. by WXINI)
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 2001, 2003, 2007, 2010, 2013, 2016
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
C----------------------------------------------------------------------
C   Does I/O to a planetary position table.  Used after setup by POINI.
C   Inputs:
C      OPCODE       C*4         Operation code:
C                               'READ' = read entry from table
C                               'WRIT' = write entry in table
C                               'CLOS' = close file, flush on write
C      WXBUFF       I(*)        I/O buffer and related storage, also
C                               defines file if open. Should have
C                               been returned by WXINI.
C   Input/Output:
C      IWXRNO       I           Next row to read or write
C      WXKOLS       I(MAXWXC)   The column pointer array in order:
C                               TIME, DTIME, IANT, TEMP, PRESS,
C                               DEWPT, WVEL, WDIR, H2OCOL, IONCOL
C      WXNUMV       I(MAXWXC)   Element count in each column.
C      TIME         D           Time (days wrt reference day)
C      DTIME        R           integration time (days)
C      IANT         I           antenna number
C      SUBA         I           subarray (0 -> any)
C      TEMP         R           surface temperature (C)
C      PRESS        R           surface pressure (mbar)
C      DEWPT        R           dew point temperature (C)
C      WVEL         R           wind velocity (m/s)
C      WDIR         R           wind direction (east from north)
C      WGUST        R           wind gusts m/sec
C      PRECIP       R           precipitation (cm)
C      H2OCOL       R           water column (m^-2)
C      IONCOL       R           ion column (m^-2)
C   Output:
C      IERR         I           Return code (0=>ok; else error)
C                               Note: -1 => read by not selected
C----------------------------------------------------------------------
      INCLUDE 'INCS:DWXV.INC'
      CHARACTER OPCODE*4
      INTEGER   WXBUFF(512), IWXRNO, WXKOLS(MAXWXC), WXNUMV(MAXWXC),
     *   IANT, SUBA, IERR
      DOUBLE PRECISION TIME
      REAL      DTIME, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL
C
      DOUBLE PRECISION RECD(MXDPWX)
      REAL      RECR(MXSPWX)
      INTEGER   RECI(MXSPWX)
      EQUIVALENCE (RECD, RECR, RECI)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IWXRNO, RECI, WXBUFF, IERR)
         IF (IERR.GT.0) CALL TABERR ('CLOS', 'TABIO', 'TABWX', IERR)
C                                       Do I/O
      ELSE
C                                       If write fill RECORD
         IF (OPCODE.NE.'READ') THEN
C                                       Time
            IF ((WXKOLS(WXDTIM).GT.0) .AND. (WXNUMV(WXDTIM).GT.0))
     *         RECD(WXKOLS(WXDTIM)) = TIME
            IF ((WXKOLS(WXDTIM).GT.0) .AND. (WXNUMV(WXDTIM).LT.0))
     *         RECR(WXKOLS(WXDTIM)) = TIME
C                                       integration Time
            IF ((WXKOLS(WXRINT).GT.0) .AND. (WXNUMV(WXRINT).GT.0))
     *         RECR(WXKOLS(WXRINT)) = DTIME
C                                       antenna
            IF ((WXKOLS(WXIANT).GT.0) .AND. (WXNUMV(WXIANT).GT.0))
     *         RECI(WXKOLS(WXIANT)) = IANT
C                                       subarray
            IF ((WXKOLS(WXSUBA).GT.0) .AND. (WXNUMV(WXSUBA).GT.0))
     *         RECI(WXKOLS(WXSUBA)) = SUBA
C                                       surface temperature
            IF ((WXKOLS(WXRTMP).GT.0) .AND. (WXNUMV(WXRTMP).GT.0))
     *         RECR(WXKOLS(WXRTMP)) = TEMP
C                                       surface pressure
            IF ((WXKOLS(WXRPRS).GT.0) .AND. (WXNUMV(WXRPRS).GT.0))
     *         RECR(WXKOLS(WXRPRS)) = PRESS
C                                       dew point temperature
            IF ((WXKOLS(WXRDWP).GT.0) .AND. (WXNUMV(WXRDWP).GT.0))
     *         RECR(WXKOLS(WXRDWP)) = DEWPT
C                                       wind velocity
            IF ((WXKOLS(WXRVEL).GT.0) .AND. (WXNUMV(WXRVEL).GT.0))
     *         RECR(WXKOLS(WXRVEL)) = WVEL
C                                       wind direction
            IF ((WXKOLS(WXRDIR).GT.0) .AND. (WXNUMV(WXRDIR).GT.0))
     *         RECR(WXKOLS(WXRDIR)) = WDIR
C                                       wind gusts
            IF ((WXKOLS(WXRGUS).GT.0) .AND. (WXNUMV(WXRGUS).GT.0))
     *         RECR(WXKOLS(WXRGUS)) = WGUST
C                                       preciptation
            IF ((WXKOLS(WXRPRE).GT.0) .AND. (WXNUMV(WXRPRE).GT.0))
     *         RECR(WXKOLS(WXRPRE)) = PRECIP
C                                       water column
            IF ((WXKOLS(WXRH2O).GT.0) .AND. (WXNUMV(WXRH2O).GT.0))
     *         RECR(WXKOLS(WXRH2O)) = H2OCOL
C                                       ion column
            IF ((WXKOLS(WXRION).GT.0) .AND. (WXNUMV(WXRION).GT.0))
     *         RECR(WXKOLS(WXRION)) = IONCOL
            END IF
C                                       Process record
         CALL TABIO (OPCODE, 0, IWXRNO, RECR, WXBUFF, IERR)
         IWXRNO = IWXRNO + 1
         IF (IERR.GT.0) THEN
            CALL TABERR (OPCODE, 'TABIO', 'TABWX', IERR)
            GO TO 999
C                                       If READ pick data from record
         ELSE IF (OPCODE.EQ.'READ') THEN
C                                       Time
            IF ((WXKOLS(WXDTIM).GT.0) .AND. (WXNUMV(WXDTIM).GT.0)) THEN
               TIME = RECD(WXKOLS(WXDTIM))
            ELSE IF ((WXKOLS(WXDTIM).GT.0) .AND. (WXNUMV(WXDTIM).LT.0))
     *         THEN
               TIME = RECR(WXKOLS(WXDTIM))
            ELSE
               TIME = DBLANK
               END IF
C                                       integration Time
            IF ((WXKOLS(WXRINT).GT.0) .AND. (WXNUMV(WXRINT).GT.0)) THEN
               DTIME = RECR(WXKOLS(WXRINT))
            ELSE
               DTIME = FBLANK
               END IF
C                                       antenna
            IF ((WXKOLS(WXIANT).GT.0) .AND. (WXNUMV(WXIANT).GT.0)) THEN
               IANT = RECI(WXKOLS(WXIANT))
            ELSE
               IANT = -1
               END IF
C                                       subarray
            IF ((WXKOLS(WXSUBA).GT.0) .AND. (WXNUMV(WXSUBA).GT.0)) THEN
               SUBA = RECI(WXKOLS(WXSUBA))
            ELSE
               SUBA = -1
               END IF
C                                       surface temperature
            IF ((WXKOLS(WXRTMP).GT.0) .AND. (WXNUMV(WXRTMP).GT.0)) THEN
               TEMP = RECR(WXKOLS(WXRTMP))
            ELSE
               TEMP = FBLANK
               END IF
C                                       surface pressure
            IF ((WXKOLS(WXRPRS).GT.0) .AND. (WXNUMV(WXRPRS).GT.0)) THEN
               PRESS = RECR(WXKOLS(WXRPRS))
            ELSE
               PRESS = FBLANK
               END IF
C                                       dew point temperature
            IF ((WXKOLS(WXRDWP).GT.0) .AND. (WXNUMV(WXRDWP).GT.0)) THEN
               DEWPT = RECR(WXKOLS(WXRDWP))
            ELSE
               DEWPT = FBLANK
               END IF
C                                       wind velocity
            IF ((WXKOLS(WXRVEL).GT.0) .AND. (WXNUMV(WXRVEL).GT.0)) THEN
               WVEL = RECR(WXKOLS(WXRVEL))
            ELSE
               WVEL = FBLANK
               END IF
C                                       wind direction
            IF ((WXKOLS(WXRDIR).GT.0) .AND. (WXNUMV(WXRDIR).GT.0)) THEN
               WDIR = RECR(WXKOLS(WXRDIR))
            ELSE
               WDIR = FBLANK
               END IF
C                                       wind gust
            IF ((WXKOLS(WXRGUS).GT.0) .AND. (WXNUMV(WXRGUS).GT.0)) THEN
               WGUST = RECR(WXKOLS(WXRGUS))
            ELSE
               WGUST = FBLANK
               END IF
C                                       precipitation
            IF ((WXKOLS(WXRPRE).GT.0) .AND. (WXNUMV(WXRPRE).GT.0)) THEN
               PRECIP = RECR(WXKOLS(WXRPRE))
            ELSE
               PRECIP = FBLANK
               END IF
C                                       water column
            IF ((WXKOLS(WXRH2O).GT.0) .AND. (WXNUMV(WXRH2O).GT.0)) THEN
               H2OCOL = RECR(WXKOLS(WXRH2O))
            ELSE
               H2OCOL = FBLANK
               END IF
C                                       ion column
            IF ((WXKOLS(WXRION).GT.0) .AND. (WXNUMV(WXRION).GT.0)) THEN
               IONCOL = RECR(WXKOLS(WXRION))
            ELSE
               IONCOL = FBLANK
               END IF
            END IF
         END IF
C
 999  RETURN
      END

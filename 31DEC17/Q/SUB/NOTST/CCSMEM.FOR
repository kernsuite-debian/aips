      SUBROUTINE CCSMEM (IFIELD, DOSUM, CHANEL, NCHAN, JBUFSZ, LOGRID,
     *   MODSTA, MODEND, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Transforms CLEAN components to a grid. - remaining in AP memory
C# AP-util Map UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2015, 2018
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
C   CCSMEM transforms CLEAN components to GRD file.
C   Input:
C      IFIELD   I      FIELD number for grid.
C      DOSUM    L      If true sum the flux in each field
C      CHANEL   I      First channel to subtract in uv data
C      NCHAN    I      Number channels to subtract
C      JBUFSZ   I      Size of the buffers in bytes. The dimension of
C                      the buffers must be at least 4096.
C      LOGRID   I      AP pointer for start of FFTed model grid
C      MODSTA   I       First model to use
C      MODEND   I       Last model to use
C      CLEAN components for field IFIELD.
C   Output:
C      BUFF1    R(*)   Work buffer
C      BUFF2    R(*)   Work buffer
C      BUFF3    R(*)   Work buffer
C      IRET     I      Error return
C   Transform of additional CLEAN components left in AP memory.
C   memory version of CCSGRD.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, CHANEL, NCHAN, JBUFSZ, LOGRID, MODSTA, MODEND,
     *   IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
      LOGICAL   DOSUM
C
      INTEGER   FIRST, NUMBER, MAXCMP, NUMCLN, JNUM, NAPGAU, NAPEX1,
     *   NAPGRD, NAPCMP, NAPCMS, APBUF, ONENY, TWONY, WRK1, WRK2,
     *   APSIZ, NLOAD, I, JLIM, NX, NY, CURGRD, LGRD, CCVSAV, LFIELD,
     *   TOTNUM, NEED, KAP, INPSAP
      LOGICAL   DOGAUS, WESET
      REAL      GPARMS(3), DU, DV, UU
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
C-----------------------------------------------------------------------
      INPSAP = PSAPNW
C                                       has DMOD.INC been set up?
      WESET = MODMAX.LE.0
      IF (WESET) THEN
         MODMAX = 1
         MODSTA = 1
         MODEND = 1
         MODFLD(1) = IFIELD
         MODCHN(1) = CHANEL
         MODNCH(1) = NCHAN
         MODCCV(1) = ABS(CCVER(IFIELD))
         MODCCB(1) = NSUBG(IFIELD)
         END IF
C                                       loop over requested grids
      MODGRD(MODSTA) = LOGRID
      TOTNUM = 0
      DO 100 LGRD = MODSTA,MODEND
         LFIELD = MODFLD(LGRD)
         CCVSAV = CCVER(LFIELD)
         CCVER(LFIELD) = MODCCV(LGRD)
         NX = FLDSZ(1,LFIELD) * OSFX + 0.1
         NY = FLDSZ(2,LFIELD) * OSFY + 0.1
C                                       Initialize flux sums.
         IF (DOSUM) FLUXG(LFIELD) = 0.0
C                                       Determine Area assignments in AP
         ONENY = NY
         TWONY = 2 * NY
         WRK1 = ONENY + 1
         WRK2 = TWONY + 1
         NAPGRD = MODGRD(LGRD)
         NAPCMP = NAPGRD + 2*NY*(NX/2+12) + 1
         MODGRD(LGRD+1) = NAPCMP
         NAPGAU = NAPCMP + WRK2
         NAPEX1 = NAPGAU + WRK1
         NAPCMS = NAPEX1 + WRK1
         APSIZ = PSAPNW * 1024
         MAXCMP = (APSIZ - NAPCMS) / 5
         NUMCLN = NCLNG(LFIELD) - MODCCB(LGRD) + 1
         IF (MAXCMP.LE.0) THEN
            NEED = (NAPCMS + 5 * NUMCLN - 1) / 1024 + 4 + INPSAP
            CALL QINIT (NEED, 0, KAP)
            MAXCMP = (PSAPNW*1024 - NAPCMS) / 5
            END IF
         FIRST = MODCCB(LGRD)
         APBUF = NAPCMP
C                                       clear grid area
         NLOAD = NAPCMP - NAPGRD
         CALL QVCLR (NAPGRD, 1, NLOAD)
         CALL QWR
C                                       Determine size of AP buffer for
         NLOAD = NAPCMS - NAPCMP - 3
         JLIM = NX / 2 + 1
C                                       Begin component loop.
 10      IF (FIRST.LE.NCLNG(LFIELD)) THEN
            NUMBER = MIN (MAXCMP, NUMCLN)
C                                       Load CLEAN components this pass.
            JNUM = NUMBER
            CALL GRDCRM (LFIELD, DOSUM, NAPCMS, APBUF, FIRST, NUMBER,
     *         NLOAD, GPARMS, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
            IF (IRET.EQ.10) IRET = 0
            IF (IRET.NE.0) GO TO 999
            TOTNUM = TOTNUM + NUMBER
C                                       Gaussian model?
            DOGAUS = (GPARMS(1).GT.0.0) .AND. (GPARMS(2).GT.0.0)
            IF (DOGAUS) THEN
               IF ((NX*CELLSG(1).EQ.0.0) .OR. (NY*CELLSG(2).EQ.0.0))
     *            THEN
                  IRET = 8
                  MSGTXT = 'NX, NY, OR CELLSG 0!!!'
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               DU = RAD2AS / (NX * ABS(CELLSG(1)))
               DV = RAD2AS / (NY * ABS(CELLSG(2)))
               END IF
C                                       Begin loop thru map.
            CURGRD = NAPGRD + 5 * TWONY
            DO 20 I = 1,JLIM
C                                       Gaussian taper then crunch
               IF (DOGAUS) THEN
                  UU = DU * (I-1)
                  CALL QTAPER (GPARMS, UU, DV, ONENY, NAPGAU)
                  CALL QGRDCC (NAPCMS, NAPGAU, NAPCMP, CURGRD, ONENY,
     *               NUMBER)
C                                       Crunch data.
               ELSE
                  CALL QGRDCC (NAPCMS, -1, NAPCMP, CURGRD, ONENY,
     *               NUMBER)
                  END IF
               CURGRD = CURGRD + TWONY
 20            CONTINUE
            CALL QWR
            FIRST = FIRST + JNUM
            NUMCLN = NUMCLN - JNUM
            GO TO 10
            END IF
C                                       restore CCVER
         CCVER(LFIELD) = CCVSAV
 100     CONTINUE
      IF (WESET) MODMAX = 0
      IF (TOTNUM.LE.0) IRET = 10
C
 999  RETURN
      END

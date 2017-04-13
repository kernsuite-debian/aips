      SUBROUTINE CALHIS (LUN, HBUFF, IRET)
C-----------------------------------------------------------------------
C! writes history of calibration
C# subroutine calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2010, 2012, 2014
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
C   Adds information about calibration to open HI file
C   Inputs:
C      LUN     I        Logical unit number of HI file
C   In/Out:
C      HBUFF   I(256)   HI buffer in use
C   Inputs in common: DSEL.INC
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   LUN, HBUFF(256), IRET
C
      INTEGER   I1, I2, I, ITRIM, J1, J2, ISCAL
      CHARACTER HILINE*72, SOUCOD(2)*7
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA SOUCOD /'SOURCES', 'CALSOUR'/
C-----------------------------------------------------------------------
C                                       Add selection criteria:
C                                       Sources
C                                       All cal sources ?
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,2000) TSKNAM, SOUCOD(1)
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
      ELSE
         ISCAL = 2
         DO 10 I = 1,30
            IF (SOURCS(I).NE.CALSOU(I)) ISCAL = 1
 10         CONTINUE
C                                       Included or excluded?
         WRITE (HILINE,2001) TSKNAM, SOUCOD(ISCAL), NSOUWD
         IF (DOSWNT) WRITE (HILINE,2002) TSKNAM, SOUCOD(ISCAL), NSOUWD
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       1st 2 and label.
         I1 = 1
         IF (SOURCS(1)(1:1).EQ.'-') I1 = 2
         I2 = 1
         IF (SOURCS(2)(1:1).EQ.'-') I2 = 2
         WRITE (HILINE,2003) TSKNAM, SOUCOD(ISCAL), SOURCS(1)(I1:),
     *      SOURCS(2)(I2:)
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Rest of sources
         DO 20 I = 3,MIN(NSOUWD,30),2
            IF (SOURCS(I).NE.' ' .OR. SOURCS(I+1).NE.' ') THEN
               I1 = 1
               IF (SOURCS(I)(1:1).EQ.'-') I1 = 2
               J1 = ITRIM (SOURCS(I))
               I2 = 1
               IF (SOURCS(I+1)(1:1).EQ.'-') I2 = 2
               J2 = ITRIM (SOURCS(I+1))
               WRITE (HILINE,2004) TSKNAM, SOURCS(I)(I1:J1),
     *            SOURCS(I+1)(I2:J2)
               CALL HIADD (LUN, HILINE, HBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 20         CONTINUE
         END IF
C                                       QUAL, CALCODE
      WRITE (HILINE,2020) TSKNAM, SELQUA, SELCOD
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Stokes'
      WRITE (HILINE,2021) TSKNAM, STOKES
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       IF range
      WRITE (HILINE,2022) TSKNAM, BIF, EIF
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Chan range
      WRITE (HILINE,2023) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Subarray
      WRITE (HILINE,2024) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Flag table
      IF (DOFLAG) THEN
         WRITE (HILINE,2030) TSKNAM, FGVER
      ELSE
         HILINE = TSKNAM // '/ no flagging applied'
         END IF
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Calibration info
      IF (DOCAL) THEN
         WRITE (HILINE,2035) TSKNAM, CLUSE
      ELSE
         HILINE = TSKNAM // '/ no continuum calibration applied'
         END IF
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Calibrate weights?
      IF (DOWTCL) THEN
         HILINE = TSKNAM // '/ Weights calibrated'
      ELSE
         HILINE = TSKNAM // '/ Weights not calibrated'
         END IF
      CALL HIADD (LUN, HILINE, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Polzn correction
      IF (DOPOL.GT.0) THEN
         WRITE (HILINE,2036) TSKNAM, DOPOL
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (HILINE,2041) TSKNAM, PDVER
         IF (PDVER.GT.0) CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       BL table
      IF (DOBL) THEN
         WRITE (HILINE,2037) TSKNAM, BLVER
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       BP table
      IF (DOBAND.GT.0.0) THEN
         WRITE (HILINE,2038) TSKNAM, DOBAND
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (HILINE,2039) TSKNAM, BPVER
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Spectral smoothing
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,2040) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Frequency selection
      IF (FRQSEL.GT.0) THEN
         WRITE (HILINE,2045) TSKNAM, FRQSEL
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       uv range
      IF ((UVRNG(1).GT.0.0) .OR. (UVRNG(2).LT.1.E10)) THEN
         WRITE (HILINE,2050) TSKNAM, UVRNG
         CALL HIADD (LUN, HILINE, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT (A6,A7,' = ''''    /All sources selected')
 2001 FORMAT (A6,'N_',A7,'=',I5,' /Following Sources excluded:')
 2002 FORMAT (A6,'N_',A7,'=',I5,' /Following Sources included:')
 2003 FORMAT (A6,A7,'  = ''',A,''',''',A,'''')
 2004 FORMAT (A6,'          ,''',A,''',''',A,'''')
 2020 FORMAT (A6,'QUAL     =',I5,' CALCODE = ',A4)
 2021 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 2022 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 2023 FORMAT (A6,'BCHAN =',I4,', ECHAN =',I4,'/ Chan range')
 2024 FORMAT (A6,'SUBARRAY =',I4)
 2030 FORMAT (A6,'FLAGVER  =',I5,' /Flagging table used')
 2035 FORMAT (A6,'GAINUSE  =',I5,' /CL table used')
 2036 FORMAT (A6,'DOPOL = ',I2,', polarization correction made')
 2037 FORMAT (A6,'BLVER=',I3,'  / BL table # applied to data')
 2038 FORMAT (A6,'DOBAND = ',I2,'  / Type of BP correction done')
 2039 FORMAT (A6,'BPVER=',I3,'  / BP correction used BP table #')
 2040 FORMAT (A6,'SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing parms')
 2041 FORMAT (A6,'PDVER=',I3,'  / DOPOL correction used PD table #')
 2045 FORMAT (A6,'FRQSEL =',I5,'  / FREQID selected')
 2050 FORMAT (A6,'UVRANGE =',1PE11.4,' ,',1PE11.4,
     *   '  / uv range kilolambda')
      END

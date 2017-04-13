      SUBROUTINE FRQGET (VOL, SLOT, NFQ, ORDER, FQFS, IRET)
C-----------------------------------------------------------------------
C! gets frequencies from an FQ axis
C# Plot-util Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2012-2013
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
C   FQGET finds the FQID or FREQ axis and lists the actual frequencies
C   in it.
C   Inputs:
C      VOL    I      Image disk volume
C      SLOT   I      Image disk slot #
C   The image header must be in DCAT.INC
C   Output:
C      FQFS   D(*)   Frequencies
C      NFQ    I      Number frequencies output
C      ORDER  I      Order of freq: -1 descending, 1 ascending, 0 other
C      IRET   I      Error code
C-----------------------------------------------------------------------
      INTEGER   VOL, SLOT, NFQ, ORDER, IRET
      DOUBLE PRECISION FQFS(*)
C
C                                       Only <= 1 IF allowed
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   FQOFF, FOFF, NF, VER, LUN, IFQRNO, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NUMIF, FQID, IFSIDE, IREC, NREC, I, IROUND,
     *   LUNTMP, FQBUFF(512)
      DOUBLE PRECISION IFFREQ
      REAL      IFCHW, IFTBW, TEMP
      CHARACTER AXISTP(2)*8, BNDCOD*8
      DOUBLE PRECISION F0, AFREQ, BFREQ, CFREQ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA AXISTP /'FQID','FREQ'/
C-----------------------------------------------------------------------
C                                       find FQID axis
      NFQ = 0
      CALL AXEFND (8, AXISTP(1), CATBLK(KIDIM), CATH(KHCTP), FQOFF,
     *   IRET)
      IF (IRET.EQ.0) THEN
         CALL AXEFND (8, AXISTP(2), CATBLK(KIDIM), CATH(KHCTP), FOFF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) VOL, SLOT
            GO TO 980
            END IF
         IF (CATBLK(KINAX+FOFF).NE.1) THEN
            WRITE (MSGTXT,1010) VOL, SLOT, CATBLK(KINAX+FOFF)
            IRET = 2
            GO TO 980
            END IF
         NF = CATBLK(KINAX+FQOFF)
         F0 = CATD(KDCRV+FOFF) + (1.0 - CATR(KRCRP+FOFF)) *
     *      CATR(KRCIC+FOFF)
C                                       open FQ table
         VER = 1
         LUN = LUNTMP (1)
         NUMIF = 1
         CALL FQINI ('READ', FQBUFF, VOL, SLOT, VER, CATBLK, LUN,
     *      IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
         NREC = FQBUFF(5)
         IF (NREC.NE.NF) THEN
            WRITE (MSGTXT,1020) VOL, SLOT, NREC, NF
            IRET = 3
            GO TO 970
            END IF
         IF (NUMIF.GT.1) THEN
            WRITE (MSGTXT,1021) VOL, SLOT, NUMIF
            IRET = 3
            GO TO 970
            END IF
C                                       read all
         CALL DFILL (NF, 0.0D0, FQFS)
         NFQ = NF
         DO 30 IREC = 1,NREC
            IFQRNO = IREC
            CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) GO TO 999
            TEMP = (FQID - CATD(KDCRV+FQOFF)) / CATR(KRCIC+FQOFF) +
     *         CATR(KRCRP+FQOFF)
            I = IROUND (TEMP)
            IF ((I.LT.1) .OR. (I.GT.NF)) THEN
               WRITE (MSGTXT,1030) VOL, SLOT, IREC, I
               IRET = 4
               GO TO 970
               END IF
            FQFS(I) = F0 + IFFREQ
 30         CONTINUE
C                                       check order
         ORDER = -2
         IRET = 0
         DO 40 I = 2,NFQ
            IFFREQ = FQFS(I) - FQFS(I-1)
            IF (IFFREQ.NE.0.0D0) THEN
               IF (ORDER.EQ.-2) THEN
                  ORDER = 1
                  IF (IFFREQ.LT.0.0D0) ORDER = -1
               ELSE IF (ORDER.EQ.-1) THEN
                  IF (IFFREQ.GT.0.0D0) THEN
                     ORDER = 0
                     GO TO 975
                     END IF
               ELSE IF (ORDER.EQ.1) THEN
                  IF (IFFREQ.LT.0.0D0) THEN
                     ORDER = 0
                     GO TO 975
                     END IF
                  END IF
               END IF
 40         CONTINUE
         GO TO 975
C                                       freq axis only
      ELSE
         CALL AXEFND (8, AXISTP(2), CATBLK(KIDIM), CATH(KHCTP), FOFF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) VOL, SLOT
            GO TO 980
            END IF
         NF = CATBLK(KINAX+FOFF)
         AFREQ = CATR(KRCIC+FOFF)
         BFREQ = CATR(KRCRP+FOFF)
         DO 60 I = 1,NF
            CFREQ = I - BFREQ
            FQFS(I) = CATD(KDCRV+FOFF) + CFREQ * AFREQ
 60         CONTINUE
         NFQ = NF
         ORDER = 1
         IF (AFREQ.LT.0.0) ORDER = -1
         GO TO 999
         END IF
C                                       close
C
 970  CALL MSGWRT (8)
 975  CALL TABFQ ('CLOS', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF, FQID,
     *   IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, VER)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DISK/SLOT',I3,I6,' HAS FQID AXIS BUT NO FREQ AXIS')
 1010 FORMAT ('DISK/SLOT',I3,I6,' HAS FQID AXIS BUT FREQ AXIS HAS',I5,
     *   ' PIXELS')
 1020 FORMAT ('DISK/SLOT',I3,I6,' HAS FQID AXIS BUT FQID AXIS AND',
     *   ' FQ TABLE',2I4,' DIFFER')
 1021 FORMAT ('DISK/SLOT',I3,I6,' HAS FQID AXIS BUT NUMIF',I4,
     *   ' GREATER THAN ONE')
 1030 FORMAT ('DISK/SLOT',I3,I6,' FQ REC',I3,' FQID POINT',I4,
     *   ' OUTSIDE AXIS')
 1050 FORMAT ('DISK/SLOT',I3,I6,' HAS NEITHER AN FQID OR A FREQ AXIS')
      END

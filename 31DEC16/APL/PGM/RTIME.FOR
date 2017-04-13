      PROGRAM RTIME
C-----------------------------------------------------------------------
C! Task to grab the AP for a specified time
C# AP-util Batch Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Task RTIME suspends itself for an amount of time specified by the
C   adverb DETIME.  The purpose of this is that AP tasks think that
C   RTIME uses the AP and while it is running tasks with higher POPS
C   numbers have reduced access to the AP.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      DETIME         DETIME        No. min to suspend before quiting.
C-----------------------------------------------------------------------
      INTEGER   INNER, OUTER
      PARAMETER (INNER = 2000000)
      PARAMETER (OUTER = 1000)
C
      CHARACTER PRGM*6
      LOGICAL   RQUICK
      INTEGER   NPARM, IRET, IERR, I, J, NCOUNT, ISUM, IT1(6), IT2(6),
     *   IFACT, IB(INNER), IB0, IOFF, ISS(OUTER)
      REAL      DETIME, BUFF(256), RSUM, CPU1, CPU2, RFACT,
     *   RB(INNER), RB0, RSS(OUTER)
      DOUBLE PRECISION JD1, JD2, RR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGM /'RTIME '/
C-----------------------------------------------------------------------
C                                       Init I/O
      CALL ZDCHIN (.TRUE., BUFF)
C                                       Get input parameters.
      NPARM = 1
      CALL GTPARM (PRGM, NPARM, RQUICK, DETIME, BUFF, IRET)
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 0
      NCOUNT = DETIME + 0.5
      IF (NCOUNT.GT.OUTER) NCOUNT = OUTER
      IF (NCOUNT.LE.0) NCOUNT = 50
      IFACT = 3000
      RFACT = 30000.
      J = -1
      DO 10 I = 1,INNER
         J = - J
         IB(I) = J
         RB(I) = J
 10      CONTINUE
      IB0 = IB(1)
      RB0 = RB(1)
      WRITE (MSGTXT,1010) NCOUNT, INNER
      CALL MSGWRT (5)
      IOFF = INNER + 1
C                                       mark time
      CALL ZDATE (IT1(1))
      CALL ZTIME (IT1(4))
      CALL ZCPU (CPU1, I)
C                                       floating
      DO 110 J = 1,NCOUNT
         RSUM = 0.0
         DO 105 I = 1,INNER
            RSUM = RSUM + RB0
 105        CONTINUE
         RSS(J) = RSUM
 110     CONTINUE
C                                       end time
      CALL ZDATE (IT2(1))
      CALL ZTIME (IT2(4))
      CALL ZCPU (CPU2, I)
C                                       report
      CALL DAT2JD (IT1, JD1)
      CALL DAT2JD (IT2, JD2)
      RR = (JD2 - JD1) * 8.64D4
      CPU1 = CPU2 - CPU1
      WRITE (MSGTXT,1120) RR, CPU1, '1 = Float sum'
      CALL MSGWRT (5)
C                                       mark time
      CALL ZDATE (IT1(1))
      CALL ZTIME (IT1(4))
      CALL ZCPU (CPU1, I)
C                                       floating
      DO 120 J = 1,NCOUNT
         RSUM = 0.0
         DO 115 I = 1,INNER
            RSUM = RSUM + RB0 - RSUM/RFACT
 115        CONTINUE
         RSS(J) = RSUM
 120     CONTINUE
C                                       end time
      CALL ZDATE (IT2(1))
      CALL ZTIME (IT2(4))
      CALL ZCPU (CPU2, I)
C                                       report
      CALL DAT2JD (IT1, JD1)
      CALL DAT2JD (IT2, JD2)
      RR = (JD2 - JD1) * 8.64D4
      CPU1 = CPU2 - CPU1
      WRITE (MSGTXT,1120) RR, CPU1, '2 = Float sum, div'
      CALL MSGWRT (5)
C                                       mark time
      CALL ZDATE (IT1(1))
      CALL ZTIME (IT1(4))
      CALL ZCPU (CPU1, I)
C                                       floating
      DO 130 J = 1,NCOUNT
         RSUM = 0.0
         DO 125 I = 1,INNER
            RSUM = RSUM + RB(I) - RSUM/RFACT + RB(IOFF-I)
 125        CONTINUE
         RSS(J) = RSUM
 130     CONTINUE
C                                       end time
      CALL ZDATE (IT2(1))
      CALL ZTIME (IT2(4))
      CALL ZCPU (CPU2, I)
C                                       report
      CALL DAT2JD (IT1, JD1)
      CALL DAT2JD (IT2, JD2)
      RR = (JD2 - JD1) * 8.64D4
      CPU1 = CPU2 - CPU1
      WRITE (MSGTXT,1120) RR, CPU1, '3 = Float sum, div, matrix'
      CALL MSGWRT (5)
C                                       mark time
      CALL ZDATE (IT1(1))
      CALL ZTIME (IT1(4))
      CALL ZCPU (CPU1, I)
C                                       integer
      DO 210 J = 1,NCOUNT
         ISUM = 0
         DO 205 I = 1,INNER
            ISUM = ISUM + IB0
 205        CONTINUE
         ISS(J) = ISUM
 210     CONTINUE
C                                       end time
      CALL ZDATE (IT2(1))
      CALL ZTIME (IT2(4))
      CALL ZCPU (CPU2, I)
C                                       report
      CALL DAT2JD (IT1, JD1)
      CALL DAT2JD (IT2, JD2)
      RR = (JD2 - JD1) * 8.64D4
      CPU1 = CPU2 - CPU1
      WRITE (MSGTXT,1120) RR, CPU1, '4 = Integer sum'
      CALL MSGWRT (5)
C                                       mark time
      CALL ZDATE (IT1(1))
      CALL ZTIME (IT1(4))
      CALL ZCPU (CPU1, I)
C                                       integer
      DO 220 J = 1,NCOUNT
         ISUM = 0
         DO 215 I = 1,INNER
            ISUM = ISUM + IB0 - ISUM/IFACT
 215        CONTINUE
         ISS(J) = ISUM
 220     CONTINUE
C                                       end time
      CALL ZDATE (IT2(1))
      CALL ZTIME (IT2(4))
      CALL ZCPU (CPU2, I)
C                                       report
      CALL DAT2JD (IT1, JD1)
      CALL DAT2JD (IT2, JD2)
      RR = (JD2 - JD1) * 8.64D4
      CPU1 = CPU2 - CPU1
      WRITE (MSGTXT,1120) RR, CPU1, '5 = Integer sum, div'
      CALL MSGWRT (5)
C                                       mark time
      CALL ZDATE (IT1(1))
      CALL ZTIME (IT1(4))
      CALL ZCPU (CPU1, I)
C                                       integer
      DO 230 J = 1,NCOUNT
         ISUM = 0
         DO 225 I = 1,INNER
            ISUM = ISUM + IB(I) - ISUM/IFACT + IB(IOFF-I)
 225        CONTINUE
         ISS(J) = ISUM
 230     CONTINUE
C                                       end time
      CALL ZDATE (IT2(1))
      CALL ZTIME (IT2(4))
      CALL ZCPU (CPU2, I)
C                                       report
      CALL DAT2JD (IT1, JD1)
      CALL DAT2JD (IT2, JD2)
      RR = (JD2 - JD1) * 8.64D4
      CPU1 = CPU2 - CPU1
      WRITE (MSGTXT,1120) RR, CPU1, '6 = Integer sum, div, matrix'
      CALL MSGWRT (5)
C                                        Write end message
 990  CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,'GETTING INPUT PARAMETERS')
 1010 FORMAT ('Iterating',I6,' times of inner loop of size',I9)
 1120 FORMAT ('Real ',F8.1,' cpu',F8.2,' for Loop ',A)
      END

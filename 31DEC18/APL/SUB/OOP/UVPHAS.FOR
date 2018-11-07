C   UVDATA Class utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "UVDATA" utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2010, 2012, 2015
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
C   Public functions:
C
C   UVPHAS (uvin, uvout, ierr)
C      Does time averaging with RMS checking
C
C  OOA Fronts to AIPS uv data specific routines:
C-----------------------------------------------------------------------
LOCAL INCLUDE 'UVSTUFF.INC'
      INTEGER   MXVS
C                                       MXVS = maximum np. correlations
C                                       in a record.
      PARAMETER (MXVS = 2048)
C                                        Local Info for uv util.
      REAL     RP(50), VS(3,MXVS)
      COMMON /UVULCM/ RP, VS
LOCAL END
      SUBROUTINE UVPHAS (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Baseline dependent time averaging of a uv data set.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C      UVOUT   C*?   Name of output uvdata object.
C   Inputs from UVIN Object.
C      MAXATIME R    Maximum averaging time in seconds
C      MINSNR   R    Maximum field of view in arc min.
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INCLUDE 'UVSTUFF.INC'
C                                       Small Vis Amplitude
      REAL    EPSILN
C                                       Max time bins, Uv storage buff
      INTEGER   MXTIME, MXBUFF, MXVIS, MXANT
      PARAMETER (EPSILN=1.0E-10, MXTIME=10000, MXBUFF=100*3*MXVS,
     *   MXVIS=10000, MXANT=30)
      CHARACTER SRTORD*2, CDUMMY*1
      INTEGER   TYPE, DIM(3), COUNT, LREC, NRPARM, NCOR, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF,
     *   ANTCNT(MXANT), NTIME, I, ANTONE, ANTTWO, PRTCNT, ANTLST, ANT1,
     *   ANT2, NANT, NCOTS, MXINTB, DUMMY
      REAL      RPA(15), VSS(3,MXVS), VSSRMS(3,MXVS), RPSAV(15,MXVIS),
     *   MXATIM, MINSNR, TIME, COTIME, COTIM2, PHSTIM, INTIME, AVETIM,
     *   VECSCL, COAVER, AVEOUT, PRTLEV, ANTBIN(MXANT), VISTOP(3,4),
     *   VISBUF(MXBUFF), TIMBIN(MXTIME), DELTIM(MXTIME), COTS(MXTIME),
     *   MDCOTM, MEDIAN
      LOGICAL   NEXT, DOSU, DOFQ, EOF, DOCOAV
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Init times
      NTIME = 0
      PRTCNT = 0
      ANTLST = 0
      NCOTS = 0
      MXINTB = 0
C                                       Request Stokes I
      DIM(1)=4
      DIM(2)=1
      DIM(3)=0
      CALL SECPUT (UVIN, 'STOKES   ', OOACAR, DIM, DUMMY, 'I   ', IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SECPUT ERROR SELECTING STOKES = I'
         CALL MSGWRT (5)
         GO TO 990
         END IF
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Chack that it's visibility data
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SRTORD(:1).NE.'U') THEN
         MSGTXT = 'UVPHAS DOES NOT WORK FOR DATA OF TYPE ''' //
     *      SRTORD // ''''
         IERR = 8
         GO TO 995
         END IF
C                                       Check that sort = B*
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SRTORD.NE.'BT') THEN
         MSGTXT = 'UVPHAS: DATA NOT IN BT ORDER, USE UVSRT'
         IERR = 5
         GO TO 995
         END IF
C                                       Create output object
      CALL OCLONE (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      COUNT = 0
C                                       Zero number of visibilities
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (UVOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Maximum time for coherent average
      CALL OUVGET (UVIN, 'MAXATIME', TYPE, DIM, MXATIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       If zero max ave time, ave all
      IF (MXATIM.LE.1.E-10) MXATIM = 1.E10
C                                       Get SNR level to flag
      CALL OUVGET (UVIN, 'MINSNR', TYPE, DIM, MINSNR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get Vector/Scalar ratio limit
      CALL OUVGET (UVIN, 'VECSCL', TYPE, DIM, VECSCL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CoherentAverage Flag
      CALL OUVGET (UVIN, 'COAVER', TYPE, DIM, COAVER, CDUMMY, IERR)
      DOCOAV = COAVER.GT.0.0
C                                       Output data timer interval
      CALL OUVGET (UVIN, 'AVEOUT', TYPE, DIM, AVEOUT, CDUMMY, IERR)
C                                       Determine amount to print
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Tell User
      WRITE (MSGTXT,1100) MXATIM
      IF (DOCOAV) CALL MSGWRT (3)
      WRITE (MSGTXT,1105) MINSNR
      CALL MSGWRT (3)
      WRITE (MSGTXT,1110) VECSCL
      CALL MSGWRT (3)
      WRITE (MSGTXT,1120) AVEOUT
      CALL MSGWRT (3)
C                                       Convert time (minutes) to days
      AVEOUT = AVEOUT / (24.*60.)
      MXATIM = MXATIM / (24.*60.)
      IF (DOCOAV) THEN
         MSGTXT = 'UVPHAS: Doing Baseline Based Coherence Time Averages'
         CALL MSGWRT (3)
         END IF
C                                       Get Uv data pointers
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Init antenna count bins
      CALL RFILL (MXANT, 0.0, ANTBIN)
      CALL FILL (MXANT, 0, ANTCNT)
C                                       Which random parameters to check
      DOSU = ILOCSU.GE.1
      DOFQ = ILOCFQ.GE.1
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of correlators
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      NCOR = MAX(NCOR, 1)
C                                       Check that it fits
      IF (NCOR.GT.MXVS) THEN
         WRITE (MSGTXT,1000) NCOR, MXVS
         IERR = 5
         GO TO 995
         END IF
C                                       Loop through input data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 990
C                                       Note end of File
         EOF = (IERR.LT.0)
C                                       New baseline
         IF (ILOCB.GT.0) THEN
            NEXT = RP(ILOCB).NE.RPSAV(ILOCB,1)
         ELSE
            NEXT = (RP(ILOCA1).NE.RPSAV(ILOCA1,1)) .OR.
     *         (RP(ILOCA2).NE.RPSAV(ILOCA2,1))
            END IF
C                                       New source
         NEXT = NEXT .OR. (DOSU .AND. (RP(ILOCSU).NE.RPSAV(ILOCSU,1)))
C                                       New FQid
         NEXT = NEXT .OR. (DOFQ .AND. (RP(ILOCFQ).NE.RPSAV(ILOCFQ,1)))
C                                       If new or end of file and data
         IF (((NEXT .OR. EOF) .AND. (NTIME.GT.0)) .OR.
     *      (NTIME.GE.MXTIME)) THEN
C                                       Average Random Parameters
            CALL RPAVE (ILOCU, ILOCV, ILOCW, ILOCT, NRPARM,
     *         1, NTIME, RPA, RPSAV)
C                                       Decode Antenna numbers
            IF (ILOCB.GT.0) THEN
               ANT1 = ANTONE(RPA(ILOCB))
               ANT2 = ANTTWO(RPA(ILOCB))
            ELSE
               ANT1 = RPA(ILOCA1) + 0.1
               ANT2 = RPA(ILOCA2) + 0.1
               END IF
C                                       Get average, RMS Vis
            CALL VRMS (NCOR, 1, NTIME, MXBUFF, VSS, VSSRMS, VISBUF)
            CALL RCOPY (12, VSS, VISTOP)
C                                       Flag wild visibilities
            CALL VFLAG (NCOR, 1, NTIME, MXBUFF, MINSNR, VSS,
     *         VSSRMS, VISBUF)
C                                       Get cooherence time
            CALL VCOHER (NCOR, 1, 1, ILOCT, 1, NTIME,
     *         VECSCL, MXBUFF, RPSAV, VISBUF, COTIME)
C                                       Fix No Coherence time case
            IF (COTIME.GT.0.0) THEN
C                                       Flag vis within coherence
               CALL VCOFLG (NCOR, ILOCT, 1, NTIME, MINSNR, MXBUFF,
     *            RPSAV, VISBUF, COTIME)
C                                       Get cooherence time again
               CALL VCOHER (NCOR, 1, 1, ILOCT, 1, NTIME,
     *            VECSCL, MXBUFF, RPSAV, VISBUF, COTIM2)
            ELSE
C                                       Else use integration as coher.
               COTIM2 = RPSAV(ILOCT,MIN(2,NTIME))-RPSAV(ILOCT,1)
               END IF
C                                       Debug
            IF ((PRTLEV.GT.PRTCNT) .OR. (ANT1.NE.ANTLST)) THEN
               WRITE (MSGTXT,1300) ANT1, ANT2, NTIME, COTIME*24*60,
     *           COTIM2*24.*60., (VSS(1,I),VSSRMS(1,I),
     *           SQRT(VSS(1,I)**2+VSS(2,I)**2)/MAX(EPSILN,VSSRMS(3,I)),
     *           I=1,1)
               CALL MSGWRT (3)
               END IF
            ANTLST = ANT1
            PRTCNT = PRTCNT + 1
C                                       Save conherence time measure
            ANTBIN(ANT1) = ANTBIN(ANT1) + COTIM2
            ANTCNT(ANT1) = ANTCNT(ANT1) + 1
            ANTBIN(ANT2) = ANTBIN(ANT2) + COTIM2
            ANTCNT(ANT2) = ANTCNT(ANT2) + 1
C                                       Record coherenc time
            IF (COTIM2.GT.0) THEN
               NCOTS = NCOTS + 1
               COTS(NCOTS) = COTIM2
            ELSE
C                                       Else use integration as coher.
               COTIM2 = RPSAV(ILOCT,MIN(2,NTIME))-RPSAV(ILOCT,1)
               END IF
C                                       If writing coherent averages
            IF ((DOCOAV) .AND. (COTIME.GT.0.)) THEN
C                                       average no longer than max allow
               AVETIM = MIN (COTIM2, MXATIM)
C                                       if coherence > ave/? then write
               IF (COTIM2.GT.AVEOUT/1.5)
C                                       Do coherent average and write
     *            CALL VCAOUT (NCOR, ILOCU, ILOCV, ILOCW, ILOCT, NRPARM,
     *               1, NTIME, MXBUFF, RPSAV, UVOUT, COUNT, AVETIM,
     *               AVEOUT, VISBUF, IERR)
            ELSE
C                                       Else, Just write unflagged data
               CALL VOUT (NCOR, 1, NTIME, MXBUFF, RPSAV, UVOUT,
     *            COUNT, VISBUF, IERR)
               END IF
            IF (IERR.GT.0) GO TO 990
C                                       If more integration times
            IF (NTIME.GT.MXINTB) THEN
C                                       Use times to calculate Deltas
               DO 800 I = 1,NTIME-1
                  DELTIM(I) = TIMBIN(I+1) - TIMBIN(I)
 800              CONTINUE
C                                       Itegration time is median delta
               I = NTIME - 1
               IF (I.GT.2) THEN
                  INTIME = MEDIAN (I, DELTIM)
               ELSE IF (I.EQ.2) THEN
                  INTIME = MIN (DELTIM(1), DELTIM(2))
               ELSE
                  INTIME = DELTIM(1)
                  END IF
               INTIME = DELTIM(MAX(1,(NTIME-1)/2))
               MXINTB = NTIME
               END IF
C                                       End if Finished average
C                                       Re init time bins
            IF (.NOT. EOF) NTIME = 0
            END IF
C                                       Exit on End of File
         IF (EOF) GO TO 200
C                                       If first, init vis buffer
C         IF (NTIME.LE.0)
C     *      CALL RFILL (MIN(MXBUFF,MXTIME*NCOR*3), 0.0, VISBUF)
C                                       Accumulate
         TIME = RP(ILOCT)
C                                       Save random parms
         CALL RPPUT (NRPARM, TIME, NTIME, TIMBIN, RP, RPSAV)
C                                       Save visability
         CALL VPUT (NCOR, NTIME, MXBUFF, VS, VISBUF)
C                                       Loop for next vis
         GO TO 100
C                                       Done
 200  IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVPHAS: NO DATA SELECTED'
         GO TO 995
      ELSE
         WRITE (MSGTXT,1250) COUNT
         CALL MSGWRT (4)
         END IF
C                                       Close files
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy tables
      CALL UVCALT (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Prepare to calculate average T
      NANT = 0
      COTIME = 0.0
      PHSTIM = 1E20
C                                       Report antenna coherence times
      DO 700 I = 1,MXANT
C                                       If counts for this antenna
         IF (ANTCNT(I).GT.0) THEN
C                                       Calc Average coherence time
            WRITE (MSGTXT,1400) I, ANTCNT(I),
     *         24.0*60.*ANTBIN(I)/ANTCNT(I)
            CALL MSGWRT (3)
            NANT = NANT + 1
            COTIME = COTIME + (ANTBIN(I)/ANTCNT(I))
            PHSTIM = MIN (PHSTIM, ANTBIN(I)/ANTCNT(I))
            END IF
 700     CONTINUE
C                                       Report times
      IF (MXINTB.GT.1) THEN
C                                       Return median delta time (Min)
         WRITE (MSGTXT,1600) INTIME*24.*60.
      ELSE
         INTIME = 0
         WRITE (MSGTXT,1510) NTIME, TIMBIN(1)
         END IF
      CALL MSGWRT (3)
C                                       Do statistical analysis of times
      IF (NCOTS.GT.3) THEN
C                                       Sort into decending order
C        CALL RSORT(NCOTS, COTS)
C                                       transfer median coherence
C                                       This was WRONG
C        MDCOTM = COTS(3*NCOTS/4)
         MDCOTM = MEDIAN (NCOTS, COTS)
C                                       use report median coherence
         WRITE (MSGTXT,1520) NCOTS, 24.*60.*MDCOTM
         CALL MSGWRT (3)
C                                       Record min phase time
         PHSTIM = MIN (MDCOTM, PHSTIM)
         END IF
C                                       Report Average coherence time
      IF (NANT.GT.0) THEN
         WRITE (MSGTXT, 1450) NANT, 24.0*60.*COTIME/NANT
         CALL MSGWRT (5)
         PHSTIM = MAX (INTIME, PHSTIM)
         WRITE (MSGTXT, 1460) NANT, 24.0*60.*PHSTIM
         CALL MSGWRT (5)
C                                       Create a keyword
         CALL OBVHKW ('UVDATA   ', 'PHASETIM', 2, IERR)
C                                       Record min coherence time
         CALL OPUT (UVOUT, 'PHASETIM', OOARE, DIM, PHSTIM, CDUMMY, IERR)
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVPHAS: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPHAS: TOO MANY CORRELATIONS ', I6,' > ', I6)
 1100 FORMAT ('UVPHAS: Maximum time period for ave. of Vis.',F11.2,
     *   ' Minutes')
 1105 FORMAT ('UVPHAS: Flagging vis with Deviation        >',F11.2,
     *   ' Sigma')
 1110 FORMAT ('UVPHAS: Coherence from Vector/Scalar Ratio <',F11.2)
 1120 FORMAT ('UVPHAS: Visibilities written at intervals of',F11.2,
     *   ' Minutes')
 1150 FORMAT ('UVPHAS: Using Correlators',I5,' to',I5,' for coherence')
 1250 FORMAT ('UVPHAS: Wrote ',I7,' visibilities')
 1300 FORMAT (I2,'-',I2,' (',I4,') T=',F6.2,',',F6.2,
     *        ' V=',4(F8.3,'+/-',F7.3,F7.2))
 1400 FORMAT ('UVPHAS: Ant.',I3,' in',I4,' Baselines:',F10.3,
     *       ' Minutes Coherence')
 1450 FORMAT ('UVPHAS:',I3,' Antennas have ',F10.3,' Average Coherence',
     *       ' Time (Min)')
 1460 FORMAT ('UVPHAS:',I3,' Antennas have ',F10.3,' Minimum Coherence',
     *       ' Time (Min)')
 1520 FORMAT ('UVPHAS:',I5,' Coher. times',F10.3,' Median  Coherence',
     *       ' Time (Min)')
 1500 FORMAT ('UVPHAS:',I10,' Int. period:',1PE12.5,' - ',E12.5,' Days')
 1510 FORMAT ('UVPHAS:',I10,' Obs. times :',1PE12.5,' Days')
 1600 FORMAT ('UVPHAS:',1PE10.2,' Integration interval (Min)')
      END
      SUBROUTINE VPUT (NCOR, ITIME, MXBUFF, VIS, VISBUF)
C-----------------------------------------------------------------------
C! copies one visability to buffer
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME   I          Time index for vis
C      MXBUFF  I          Maximum buffer size
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NCOR, ITIME, MXBUFF
C
      INCLUDE 'UVSTUFF.INC'
      REAL      VIS(3,MXVS), VISBUF(*)
C
      INTEGER   I, K, TOFF, IOFF
C-----------------------------------------------------------------------
C                                       If valid vis and time
      IF ((NCOR.GT.0) .AND. (ITIME.GT.0)) THEN
C                                       Calc time offset
         TOFF = NCOR*(ITIME-1)*3
C                                       For all correlators
         DO 10 I = 1,NCOR
C                                       Calc correlator offset
            IOFF = 3*(I-1) + TOFF
C                                       Exit on end of buffer
            IF (IOFF+3.GT.MXBUFF) GO TO 999
C                                       For Real Imaginary Weight
            DO 10 K = 1,3
               VISBUF(IOFF+K) = VIS(K,I)
 10            CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE VGET (NCOR, ITIME, MXBUFF, VIS, VISBUF)
C-----------------------------------------------------------------------
C   copies one visibility from buffer
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME   I          Time index for vis
C      MXBUFF  I          Maximum buffer size
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NCOR, ITIME, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      VIS(3,MXVS), VISBUF(MXVS)
C
      INTEGER   I, K, TOFF, IOFF
C-----------------------------------------------------------------------
C                                       If channels in data
      IF ((NCOR.GT.0) .AND. (ITIME.GT.0)) THEN
C                                       Calc time offset
         TOFF = NCOR*(ITIME-1)*3
C                                       For all correlators
         DO 10 I = 1,NCOR
C                                       Calc correlator offset
            IOFF = 3*(I-1) + TOFF
C                                       Exit on end of buffer
            IF (IOFF+3.GT.MXBUFF) GOTO 999
C                                       For Real, Imaginary, Weight
            DO 10 K = 1,3
               VIS(K,I) = VISBUF(IOFF+K)
 10            CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE VDEL (NCOR, ITIME, NTIME, VIS, VISBUF)
C-----------------------------------------------------------------------
C! deletes on visibility from the buffer
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME   I          Time index for vis
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NCOR, ITIME, NTIME
      INCLUDE 'UVSTUFF.INC'
      REAL      VIS(3,MXVS), VISBUF(MXVS)
C
      INTEGER   J, TOFF, JOFF
C-----------------------------------------------------------------------
C                                       If channels in data
      IF ((NCOR.GT.0) .AND. (ITIME.GT.0) .AND. (ITIME.LE.NTIME)) THEN
C                                       Calc time offset
         TOFF = NCOR*(ITIME-1)*3
C                                       For all other vis + 1 blank vis
         DO 100 J = ITIME+1,NTIME+1
C                                       Calc  Vis offset
            JOFF = TOFF + 3*NCOR*(J-1)
C                                       Overwrite previous vis
            CALL COPY (NCOR*3, VISBUF(JOFF+3*NCOR), VISBUF(JOFF))
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE VAVE (NCOR, ITIME1, ITIME2, MXBUFF, VIS, VISBUF)
C-----------------------------------------------------------------------
C! Vector average buffer visabilities to VIS
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      MXBUF   I          Maximum buffer size
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NCOR, ITIME1, ITIME2, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      VIS(3,MXVS), VISBUF(MXVS)
C
      INTEGER   I, J
      REAL      VIS2(3,MXVS), WEIGHT
C-----------------------------------------------------------------------
      IF ((NCOR.GT.0) .AND. (ITIME1.GT.0) .AND. (ITIME2.GE.ITIME1)) THEN
C                                       Zero output vis
         CALL VZERO (NCOR, VIS)
C                                       For all vis.
         DO 200 J = ITIME1,ITIME2
C                                       Get vis
            CALL VGET (NCOR, J, MXBUFF, VIS2, VISBUF)
C                                       For all correlators
            DO 100 I = 1,NCOR
C                                       if positive weight
               WEIGHT = VIS2(3,I)
               IF (WEIGHT.GT.0.) THEN
                  VIS(1,I) = VIS(1,I) + VIS2(1,I)*WEIGHT
                  VIS(2,I) = VIS(2,I) + VIS2(2,I)*WEIGHT
                  VIS(3,I) = VIS(3,I) + WEIGHT
                  END IF
 100           CONTINUE
C                                       End for all vis loop
 200        CONTINUE
C                                       For all correlators
            DO 300 I = 1,NCOR
C                                       If positive count
               IF (VIS(3,I).GT.0.0) THEN
C                                       Rescale
                  VIS(1,I) = VIS(1,I) / VIS(3,I)
                  VIS(2,I) = VIS(2,I) / VIS(3,I)
                  END IF
 300           CONTINUE
C                                       If ok command
         END IF
      RETURN
      END
      SUBROUTINE VRMS (NCOR, ITIME1, ITIME2, MXBUFF, VIS, VIS2, VISBUF)
C-----------------------------------------------------------------------
C! Vector RMS average buffer visabilities to VIS
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      MXBUF   I          Maximum buffer size
C      VIS     R(3,NCOR)  Average Vis
C      VIS2    R(3,NCOR)  RMS Vis,  VIS(3,*) = Scalar Amplitude
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NCOR, ITIME1, ITIME2, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      VIS(3,MXVS), VIS2(3,MXVS), VISBUF(MXVS)
C
      INTEGER   I, J, VISCNT(MXVS)
      REAL      VIS3(3,MXVS), ONEOVN
C-----------------------------------------------------------------------
      IF ((NCOR.GT.0) .AND. (ITIME1.GT.0) .AND. (ITIME2.GE.ITIME1)) THEN
C                                       Init correlator counts
         CALL FILL (NCOR, 0, VISCNT)
C                                       Zero output vis
         CALL VZERO (NCOR, VIS)
C                                       Zero square sum vis
         CALL VZERO (NCOR, VIS2)
C                                       For all vis.
         DO 200 J = ITIME1,ITIME2
C                                       Get vis
            CALL VGET (NCOR, J, MXBUFF, VIS3, VISBUF)
C                                       For all correlators
            DO 100 I = 1,NCOR
C                                       if positive weight
               IF (VIS3(3,I).GT.0.) THEN
C                                       Sum for Average
                  VIS(1,I) = VIS(1,I) + VIS3(1,I)
                  VIS(2,I) = VIS(2,I) + VIS3(2,I)
                  VIS(3,I) = VIS(3,I) + VIS3(3,I)
C                                       Sum for square average
                  VIS2(1,I) = VIS2(1,I) + VIS3(1,I)**2
                  VIS2(2,I) = VIS2(2,I) + VIS3(2,I)**2
C                                       Calc Scalar Amplitude
                  VIS2(3,I) = VIS2(3,I) +
     *                        SQRT(VIS3(1,I)**2 + VIS3(2,I)**2)
C                                       Increment count
                  VISCNT(I) = VISCNT(I) + 1
                  END IF
C                                       End for all correlatores loop
 100           CONTINUE
C                                       End for all vis loop
 200        CONTINUE
C                                       For all correlators
            DO 300 I = 1,NCOR
C                                       If positive count
               IF (VISCNT(I).GT.0) THEN
C                                       ONEOVN
                  ONEOVN = 1.0 / VISCNT(I)
C                                       calc average
                  VIS(1,I) = VIS(1,I) * ONEOVN
                  VIS(2,I) = VIS(2,I) * ONEOVN
C                                       calc RMS**2
                  VIS2(1,I) = (VIS2(1,I) * ONEOVN) - VIS(1,I)**2
                  VIS2(2,I) = (VIS2(2,I) * ONEOVN) - VIS(2,I)**2
C                                       calc RMS from RMS**2
                  IF (VISCNT(I).GT.1) THEN
                     VIS2(1,I) = SQRT(ABS(VIS2(1,I)))
                     VIS2(2,I) = SQRT(ABS(VIS2(2,I)))
                  ELSE
                     VIS2(1,I) = 1.E20
                     VIS2(2,I) = 1.E20
                     END IF
C                                       calc average scalar amplitude
                  VIS2(3,I) = VIS2(3,I) * ONEOVN
C                                       Else no data, set RMS large
               ELSE
                  VIS2(1,I) = 1.E20
                  VIS2(2,I) = 1.E20
                  END IF
 300           CONTINUE
C                                       If ok command
         END IF
      RETURN
      END
      SUBROUTINE VSRATI (NCOR, CORSTR, CORSTP, ITIME1, ITIME2, ITIME3,
     *   VECSCL, MXBUFF, VISBUF)
C-----------------------------------------------------------------------
C! Vector/Scalar average ratio limit.  This routine finds the time index
C  at which the Vector/Scalar amplitude ratio drops below VECSCL
C   Inputs:
C      NCOR    I          Number of stokes in vis
C      CORSTR  I          Start correlator for coherence check
C      CORSTP  I          Stop  correlator for coherence check
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      VECSCL  I          Vector/scalar average limit
C      MXBUF   I          Maximum buffer size
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C   In/Output:
C      ITIME3  I          Coherence time limit
C-----------------------------------------------------------------------
      INTEGER   NCOR, CORSTR, CORSTP, ITIME1, ITIME2, ITIME3, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      VECSCL, VISBUF(*)
C
      INTEGER   I, J, VISCNT(MXVS)
      REAL      VIS(3,MXVS), VIS3(3,MXVS)
C-----------------------------------------------------------------------
C                                       Init time index to last time
      ITIME3 = ITIME2
C                                       If valid vis
      IF ((NCOR.GT.0) .AND. (ITIME1.GT.0) .AND. (ITIME2.GT.ITIME1)) THEN
C                                       Init correlator counts
         CALL FILL (NCOR, 0, VISCNT)
C                                       Zero output vis
         CALL VZERO (NCOR, VIS)
C                                       For all vis.
         DO 200 J = ITIME1,ITIME2
C                                       Get vis
            CALL VGET (NCOR, J, MXBUFF, VIS3, VISBUF)
C                                       For parallel all correlators
            DO 100 I = CORSTR,CORSTP
C                                       if positive weight
               IF (VIS3(3,I).GT.0.) THEN
C                                       Sum for Vector Average
                  VIS(1,I) = VIS(1,I) + VIS3(1,I)
                  VIS(2,I) = VIS(2,I) + VIS3(2,I)
C                                       Calc Scalar Amplitude
                  VIS(3,I) = VIS(3,I) +
     *                        SQRT(VIS3(1,I)**2 + VIS3(2,I)**2)
C                                       Increment count
                  VISCNT(I) = VISCNT(I) + 1
C                                       If more than 1 vis.
                  IF (VISCNT(I).GT.1) THEN
C                                       See if limit is exceeded.
                     IF (SQRT(VIS(1,I)**2 + VIS(2,I)**2).LT.
     *                   VECSCL*VIS(3,I)) THEN
C                                       Current Vis -1 is limit.
                        ITIME3 = MAX (J-1, ITIME1)
C                                       Exit, limit found
                        GO TO 300
                        END IF
                     END IF
C                                       End if more than 1 point
                  END IF
C                                       End for all correlatores loop
 100           CONTINUE
C                                       End for all vis loop
 200        CONTINUE
C                                       Jump here on limit exceeded
 300     CONTINUE
C                                       End if valid request
      END IF
      RETURN
      END
      SUBROUTINE RPPUT (NRPARM, TIME, NTIME, TIMBIN, RPARM, RPBUF)
C-----------------------------------------------------------------------
C! Select time bin and put the random parameter in a buffer
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME   I          Time index for vis
C      MXBUFF  I          Maximum buffer size
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NRPARM, NTIME
      REAL      TIME, RPARM(*), RPBUF(15,*), TIMBIN(*)
C-----------------------------------------------------------------------
C                                       A new time, count it
      NTIME = NTIME + 1
      TIMBIN(NTIME) = TIME
C                                       If valid random parm
      IF ((NRPARM.GT.0) .AND. (NTIME.GT.0)) THEN
C                                       First vis random parms
         CALL RCOPY (NRPARM, RPARM, RPBUF(1,NTIME))
         END IF
C
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE RPDEL (NRPARM, ITIME, NTIME, TIMBIN, RPBUF)
C-----------------------------------------------------------------------
C! Delete random parameter from list
C   Inputs:
C      NPARM   I          Number of stokes in vis
C                         or Init zero flag
C      ITIME   I          Time index for vis
C      MXBUFF  I          Maximum buffer size
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   NRPARM, ITIME, NTIME
      REAL      RPBUF(15,*), TIMBIN(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
C                                       If not at end of list
      IF (ITIME.LT.NTIME) THEN
C                                       for all later vis
         DO 100 I = ITIME+1,NTIME
C                                       copy over previous random parms
            CALL COPY (NRPARM, RPBUF(1,I), RPBUF(1,I-1))
C                                       copy over prevous time bin
            TIMBIN(I-1) = TIMBIN(I)
 100        CONTINUE
         END IF
C                                       reset total times count
      NTIME = NTIME - 1
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE RPAVE (ILOCU, ILOCV, ILOCW, ILOCT, NRPARM,
     *   ITIME1, ITIME2, RPARM, RPBUF)
C-----------------------------------------------------------------------
C! Select time bin and put the random parameter in a buffer
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      MXBUFF  I          Maximum buffer size
C      VIS     R(3,NCOR)  Vis
C   In/Output:
C      VISBUF  R(NREAL)   Output array
C-----------------------------------------------------------------------
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, NRPARM, ITIME1, ITIME2
      REAL      RPARM(*), RPBUF(15,*)
C
      INTEGER   I
C-----------------------------------------------------------------------
C                                       If valid random parm
      IF ((NRPARM.GT.0) .AND. (ITIME1.GT.0) .AND. (ITIME2.GE.ITIME1))
     *   THEN
C                                       Init output vis
         CALL COPY (NRPARM, RPBUF(1,ITIME1), RPARM)
C                                       If more than 1 vis
         IF (ITIME2-ITIME1.GT.0) THEN
C                                       Sum visibiliities
            DO 100 I = ITIME1+1,ITIME2
C                                       Add elements
               RPARM(ILOCU) = RPARM(ILOCU) + RPBUF(ILOCU,I)
               RPARM(ILOCV) = RPARM(ILOCV) + RPBUF(ILOCV,I)
               RPARM(ILOCW) = RPARM(ILOCW) + RPBUF(ILOCW,I)
               RPARM(ILOCT) = RPARM(ILOCT) + RPBUF(ILOCT,I)
 100           CONTINUE
C                                       scale for average
            RPARM(ILOCU) = RPARM(ILOCU)/(ITIME2 - ITIME1 + 1.0)
            RPARM(ILOCV) = RPARM(ILOCV)/(ITIME2 - ITIME1 + 1.0)
            RPARM(ILOCW) = RPARM(ILOCW)/(ITIME2 - ITIME1 + 1.0)
            RPARM(ILOCT) = RPARM(ILOCT)/(ITIME2 - ITIME1 + 1.0)
C                                       End if more than 1 vis
            END IF
         END IF
C
      RETURN
C-----------------------------------------------------------------------
      END
      INTEGER FUNCTION ANTONE(BASELN)
      REAL BASELN
C                                       Crack First antenna number
      ANTONE = INT(BASELN)/256
      RETURN
C-----------------------------------------------------------------------
      END
      INTEGER FUNCTION ANTTWO(BASELN)
C-----------------------------------------------------------------------
      REAL BASELN
C                                       Crack second antenna number
      ANTTWO = MOD(INT(BASELN),256)
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE VFLAG (NCOR, ITIME1, ITIME2, MXBUFF, MINSNR, VISAVE,
     *   VISRMS, VISBUF)
C-----------------------------------------------------------------------
C! Vector/Scalar average ratio limit.  This routine finds the time index
C  at which the Vector/Scalar amplitude ratio drops below VECSCL
C   Inputs:
C      NCOR    I          Number of stokes in vis
C                         or Init zero flag
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      VECSCL  I          Vector/scalar average limit
C      MXBUF   I          Maximum buffer size
C      VISAVE  R(NREAL)   Average visibility buffer
C      VISRMS  R(NREAL)   RMS visibility buffer
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C---------------------------------------------------------------------
      INTEGER   NCOR, ITIME1, ITIME2, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      MINSNR, VISAVE(3,MXVS), VISRMS(3,MXVS), VISBUF(*)
C
      REAL      EPSILN
C                                       Small Vis Amplitude
      PARAMETER (EPSILN=1.0E-10)
      INTEGER   I, LOOP
      LOGICAL   ONZE, FLAGED
      REAL      VIS(3,MXVS)
      SAVE      ONZE
      DATA      ONZE/.FALSE./
C-----------------------------------------------------------------------
C                                       For all visibilities
      DO 200 LOOP = ITIME1,ITIME2
C                                       Get vis
         CALL VGET (NCOR, LOOP, MXBUFF, VIS, VISBUF)
C                                       Assume no flagging
         FLAGED = .FALSE.
C                                       If SNR is sufficient
         DO 100 I = 1,NCOR
C                                       If RMS known and unflagged
            IF ((VISRMS(1,I).GT.EPSILN) .AND.
     *         (VISRMS(2,I).GT.EPSILN) .AND. (VIS(3,I).GT.0.0)) THEN
C                                       If visibility exceeds minrms
               IF ((ABS(VIS(1,I)-VISAVE(1,I)).GT.MINSNR*VISRMS(1,I))
     *            .OR.
     *            (ABS(VIS(2,I)-VISAVE(2,I)).GT.MINSNR*VISRMS(2,I)))
     *            THEN
                  VIS(3,I) = -VIS(3,I)
                  FLAGED = .TRUE.
                  END IF
C                                       End if Data exceeds limits
               END IF
C                                       If an unflaged vis, keep data
 100        CONTINUE
            ONZE = .TRUE.
C                                       If data changed, put it back
            IF (FLAGED) CALL VPUT (NCOR, LOOP, MXBUFF, VIS, VISBUF)
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE VOUT (NCOR, ITIME1, ITIME2, MXBUFF, RPSAV, UVOUT,
     *   COUNT, VISBUF, IERR)
C-----------------------------------------------------------------------
C! Write out Visibilities for one baseline
C   Inputs:
C      NCOR    I          Number of stokes in vis
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      MXBUF   I          Maximum buffer size
C      RPSAV   R(*)       Average random parameters
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C      UVOUT   C*?        Name of output data object
C   In/Output:
C      COUNT   I          Count of written visibilities
C   Output:
C      IERR    I          Error return code, 0=OK
C---------------------------------------------------------------------
      INTEGER   NCOR, ITIME1, ITIME2, MXBUFF, COUNT, IERR
      CHARACTER UVOUT*(*)
      INCLUDE 'UVSTUFF.INC'
      REAL      RPSAV(15,*), VISBUF(*)
C
      INTEGER   LOOP
      REAL      VIS(3,MXVS)
C-----------------------------------------------------------------------
C                                       For all visibilities
      DO 100 LOOP = ITIME1,ITIME2
C                                       Get vis
         CALL VGET (NCOR, LOOP, MXBUFF, VIS, VISBUF)
C                                       Write it
         CALL V1OUT (NCOR, MXBUFF, RPSAV(1,LOOP), VIS, UVOUT,
     *   COUNT, VISBUF, IERR)
C
 100     CONTINUE
 999  RETURN
      END
      SUBROUTINE V1OUT (NCOR, MXBUFF, RPARM, VIS, UVOUT,
     *   COUNT, VISBUF, IERR)
C-----------------------------------------------------------------------
C! Writes out 1 random parameter, Visibility pair for one baseline
C   Inputs:
C      NCOR    I          Number of stokes in vis
C      MXBUF   I          Maximum buffer size
C      RPARM   R(*)       Random parameters
C      VIS     R(*)       Visibility
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C      UVOUT   C*?        Name of output data object
C   In/Output:
C      COUNT   I          Count of written visibilities
C   Output:
C      IERR    I          Error return code, 0=OK
C---------------------------------------------------------------------
      INTEGER   NCOR, MXBUFF, COUNT, IERR
      CHARACTER UVOUT*(*)
      INCLUDE 'UVSTUFF.INC'
      REAL      RPARM(15), VIS(3,MXVS), VISBUF(*)
C
      INTEGER   I
      LOGICAL   FLAGED
C-----------------------------------------------------------------------
C                                       Assume flagged
      FLAGED = .TRUE.
C                                       If SNR is sufficient
      DO 100 I = 1,NCOR
C                                       If RMS known and unflagged
         IF (VIS(3,I).GT.0.0) FLAGED = .FALSE.
 100     CONTINUE
C                                       If unflagged element
      IF (.NOT.FLAGED) THEN
         COUNT = COUNT + 1
         CALL UVWRIT (UVOUT, RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 999
         END IF
 999  RETURN
      END
      SUBROUTINE VCAOUT (NCOR, ILOCU, ILOCV, ILOCW, ILOCT, NRPARM,
     *   ITIME1, ITIME2, MXBUFF, RPSAV, UVOUT, COUNT, AVETIM, AVEOUT,
     *   VISBUF, IERR)
C-----------------------------------------------------------------------
C! Write out Visibilities for one baseline
C   Inputs:
C      NCOR    I          Number of stokes in vis
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      MXBUF   I          Maximum buffer size
C      RPSAV   R(*)       Average random parameters
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C      UVOUT   C*?        Name of output data object
C      AVETIM  R          Average time (days) for baseline
C      AVEOUT  R          Interval to write out UV data (time between vis)
C   In/Output:
C      COUNT   I          Count of written visibilities
C   Output:
C      IERR    I          Error return code, 0=OK
C---------------------------------------------------------------------
      INTEGER   NCOR, ILOCU, ILOCV, ILOCW, ILOCT, NRPARM,
     *   ITIME1, ITIME2, MXBUFF, COUNT, IERR
      CHARACTER UVOUT*(*)
      INCLUDE 'UVSTUFF.INC'
      REAL      RPSAV(15, *), AVETIM, AVEOUT, VISBUF(*)
C
      INTEGER   J, LOOP, ISTART, ISTOP
      REAL      VIS(3,MXVS), RPARM(15), AVETM2, TIME, TSTART, TSTOP,
     *   FSTIME, LSTIME, PRETIM
C-----------------------------------------------------------------------
C                                       Average 1/2 range, time limit
      AVETM2 = AVETIM / 2.0
      FSTIME = RPSAV(ILOCT,ITIME1)
      LSTIME = RPSAV(ILOCT,ITIME2)
      PRETIM = -1E20
C                                       For all visibilities
      DO 300 LOOP = ITIME1,ITIME2
C                                       Get current time
         TIME = RPSAV(ILOCT,LOOP)
C                                       IF Time to aver and output
         IF (TIME-PRETIM.GE.AVEOUT) THEN
C                                       Average and output
            ISTART = 0
            ISTOP  = 0
C                                       If at begin or end of times
            IF ((TIME-AVETM2.LE.FSTIME) .OR. (LOOP.LE.ITIME1)) THEN
C                                       At beginning
               TSTART = FSTIME
               ISTART = ITIME1
               TSTOP  = MIN(FSTIME + AVETM2, LSTIME)
               END IF
C                                       If at end of time
            IF ((TIME+AVETM2.GE.LSTIME) .OR. (LOOP.GE.ITIME2)) THEN
               TSTOP = LSTIME
               ISTOP = ITIME2
               TSTART = MAX(LSTIME - AVETM2, FSTIME)
               END IF
C                                       If Start not set
            IF (ISTART.LE.0) THEN
C                                       Get start, stop times
               TSTART = TIME - AVETM2
               ISTART = ITIME1
C                                       Find start time, go backwards
               DO 100 J = LOOP-1, ITIME1, -1
C                                       If time before start time
                  IF (RPSAV(ILOCT,J).LT.TSTART) THEN
C
                     ISTART = J + 1
C                                       Exit when time found
                     GO TO 110
                     END IF
 100              CONTINUE
 110           CONTINUE
C                                       End if setting start
               END IF
C                                       if stop not set
            IF (ISTOP.LE.0) THEN
               TSTOP  = TIME + AVETM2
               ISTOP  = ITIME2
               DO 200 J = LOOP+1,ITIME2
C                                       If time after stop time
                  IF (RPSAV(ILOCT,J).GT.TSTOP) THEN
C
                     ISTOP = J - 1
C                                       Exit when time found
                     GO TO 210
                     END IF
 200              CONTINUE
 210           CONTINUE
C                                       End if setting stop
               END IF
C                                       Average random parms
            CALL RPAVE (ILOCU, ILOCV, ILOCW, ILOCT, NRPARM,
     *      ISTART, ISTOP, RPARM, RPSAV)
C                                       Reset time to current time
            RPARM(ILOCT) = TIME
C                                       Now average vis
            CALL VAVE (NCOR, ISTART, ISTOP, MXBUFF, VIS, VISBUF)
C                                       Write out result
            CALL V1OUT (NCOR, MXBUFF, RPARM, VIS, UVOUT,
     *         COUNT, VISBUF, IERR)
C                                       record time last written
            PRETIM = TIME
C                                       End if time to write
            END IF
C                                       End for all vis loop
 300     CONTINUE
      RETURN
      END
      SUBROUTINE VCOHER (NCOR, CORSTR, CORSTP, ILOCT, ITIME1, ITIME2,
     *   VECSCL, MXBUFF, RPSAV, VISBUF, COTIME)
C-----------------------------------------------------------------------
C! Get average coherence time
C   Inputs:
C      NCOR    I          Number of stokes in vis
C      CORSTR  I          Start correlator for coherence check
C      CORSTP  I          Stop  correlator for coherence check
C      ILOCT   I          Index to location of Time in random param.
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      VECSCL  R          Vector/Scalar Average limit
C      MXBUF   I          Maximum buffer size
C      RPSAVF  R(15,MXVIS)Random paramter buffer
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C   Output:
C      COTIME  R          Average coherence time
C---------------------------------------------------------------------
      INTEGER   NCOR, CORSTR, CORSTP, ILOCT, ITIME1, ITIME2, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      VECSCL, RPSAV(15,*), COTIME, VISBUF(*)
C
      INTEGER   I, LOOP, NCOTIM, ICOTIM, ILAST
      LOGICAL   FLAGED
      REAL      VIS(3,MXVS)
C-----------------------------------------------------------------------
C                                       Init count of coherence times
      NCOTIM = 0
      COTIME = 0.
      ILAST  = 0
C                                       For all visibilities
      DO 200 LOOP = ITIME1,ITIME2
C                                       Get vis
         CALL VGET (NCOR, LOOP, MXBUFF, VIS, VISBUF)
C                                       Assume flagged
         FLAGED = .TRUE.
C                                       for all coherence correlators
         DO 100 I = CORSTR,CORSTP
C                                       If unflagged
            IF (VIS(3,I).GT.0.0) FLAGED = .FALSE.
 100        CONTINUE
C                                       If unflagged element
         IF ((.NOT.FLAGED) .AND. (LOOP.GT.ILAST)) THEN
C                                       Get coherence time
            CALL VSRATI (NCOR, CORSTR, CORSTP, LOOP, ITIME2, ICOTIM,
     *         VECSCL, MXBUFF, VISBUF)
C                                       If limit was found
            IF ((ICOTIM.GT.LOOP) .AND. (ICOTIM.LE.ITIME2)) THEN
C                                       Add coherence time to average
               COTIME = RPSAV(ILOCT, ICOTIM) - RPSAV(ILOCT, LOOP) +
     *                  COTIME
               NCOTIM = NCOTIM + 1
               END IF
C                                       Record end of last Coherence
            ILAST = ICOTIM
C                                       End if unflagged vis
            END IF
 200     CONTINUE
C                                       If points in average
      IF (NCOTIM.GT.0) THEN
         COTIME = COTIME / NCOTIM
      ELSE
C                                       Else use total time range
         IF (ITIME2.GT.ITIME1) THEN
            COTIME = RPSAV(ILOCT, ITIME1+1) - RPSAV(ILOCT,ITIME1)
            END IF
         END IF
 999  RETURN
      END
      SUBROUTINE VCOFLG (NCOR, ILOCT, ITIME1, ITIME2, MINSNR,
     *   MXBUFF, RPSAV, VISBUF, COTIME)
C-----------------------------------------------------------------------
C! Gets RMS and Flags visibilities within 1 coherence time
C   Inputs:
C      NCOR    I          Number of stokes in vis
C      ILOCT   I          Index to location of Time in random param.
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      MINSNR  R          Maximum diviation from Mean (Sigma)
C      MXBUF   I          Maximum buffer size
C      RPSAV   R(15,MXVIS)Random paramter buffer
C      COTIME  R          Average coherence time
C Input/Output:
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C-----------------------------------------------------------------------
      INTEGER   NCOR, ILOCT, ITIME1, ITIME2, MXBUFF
      INCLUDE 'UVSTUFF.INC'
      REAL      MINSNR, RPSAV(15,*), COTIME, VISBUF(*)
C
      INTEGER   LOOP, ISTART, ISTOP
      REAL      TSTART, TSTOP, VSS(3,MXVS), VSSRMS(3,MXVS)
C-----------------------------------------------------------------------
C                                       Only Flag if more than 2 vis
      IF (ABS(ITIME2-ITIME1).GT.2) THEN
C                                       Init average loop
         ISTART = ITIME1
         TSTART = RPSAV(ILOCT,ISTART)
C                                       For all visibilities
         DO 100 LOOP = ITIME1+1,ITIME2
            TSTOP = RPSAV(ILOCT, LOOP)
            ISTOP = LOOP - 1
C                                       If stop time > coherence
            IF (ABS(TSTOP-TSTART).GT.COTIME) THEN
C                                       If near end use all
               IF (ABS(LOOP-ITIME2).LT.3) ISTOP = ITIME2
C                                       If enough points, flag
               IF (ISTOP-ISTART.GT.2) THEN
C                                       Get average, RMS Vis
                  CALL VRMS (NCOR, ISTART, ISTOP, MXBUFF, VSS,
     *               VSSRMS, VISBUF)
C                                       Flag wild visibilities
                  CALL VFLAG (NCOR, ISTART, ISTOP, MXBUFF, MINSNR,
     *               VSS, VSSRMS, VISBUF)
C                                       End if enough points
               END IF
C                                       Reset start index
            ISTART = MIN (ISTOP+1, ITIME2)
            TSTART = RPSAV(ILOCT,ISTART)
C                                       End if coherence exceeded
            END IF
 100     CONTINUE
C                                       End if enough visibilities
         END IF
      RETURN
      END
      SUBROUTINE VZERO (NCOR, VIS)
C-----------------------------------------------------------------------
C   Zero the visibility array
C   Input:
C      NCOR   I            Number of correlators to zero
C   Output:
C      VIS    R((3,NCOR)   Visibility array, zeroed
C-----------------------------------------------------------------------
      INTEGER   NCOR
      REAL      VIS(3,*)
C
      INCLUDE 'UVSTUFF.INC'
      INTEGER   I
C-----------------------------------------------------------------------
      DO 100 I = 1,NCOR
         VIS(1,I) = 0.0
         VIS(2,I) = 0.0
         VIS(3,I) = 0.0
 100     CONTINUE
C
 999  RETURN
      END

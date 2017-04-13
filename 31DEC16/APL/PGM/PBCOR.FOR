LOCAL INCLUDE 'PBCOR.INC'
C                                       Local include for PBCOR
      INTEGER   LDEP(7)
      REAL      XPIX0, YPIX0, XINC, YINC
      DOUBLE PRECISION    RAD0, DECD0, FRL0, POL0, COSR, SINR, RREFR,
     *   DREFR, RPTR, DPTR, DPTRC, DPTRS
      COMMON /PBCHDR/ RAD0, DECD0, FRL0, POL0, COSR, SINR, RREFR, DREFR,
     *   RPTR, DPTR, DPTRC, DPTRS, XPIX0, YPIX0, XINC, YINC, LDEP
LOCAL END
LOCAL INCLUDE 'PBCOR2.INC'
C                                       output catalog header common
      INTEGER   CATOUT(256)
      REAL      CATOR(256)
      HOLLERITH CATOH(256)
      DOUBLE PRECISION CATOD(128)
      COMMON /MAPOUT/ CATOUT
      EQUIVALENCE (CATOUT, CATOR, CATOH, CATOD)
LOCAL END
      PROGRAM PBCOR
C-----------------------------------------------------------------------
C! Task to correct an image for the primary beam response.
C# Map-util Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000-2001, 2003, 2006, 2009, 2013
C;  Copyright (C) 2015-2016
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
C   PBCOR corrects an image for the primary beam  response.
C   INPUTS:
C      USERID                   Owner of the image
C      INNAME(3)                Image name (name)
C      INCLASS(2)               Image name (class)
C      INSEQ                    Image name (seq. #)
C      INDISK                   Disk unit # of image
C      BLC(7)                   Bottom left corner to model
C      TRC(7)                   Top right corner to model
C      OUTNAME (3)              Ouput image name (name)
C      OUTCLASS(2)              Output image name (class)
C      OUTSEQ                   Output image name (seq. #)
C      OUTDISK                  Output image disk
C      DPARM.......Control parameters:
C             (1)..Primary beam cutoff level.  0 => .023.  Pixels
C                  having lower sensitivity are blanked.
C             (2)..How accurate should the angle computations be?
C                  > 0 => always accurate (slow), = 0 => program
C                  chooses based on geometry and image size, < 0
C                  => always use linear.
C             (3)..> 0 => use GPOS to provide the pointing position.
C                  <= 0 => use the header pointing position (or if
C                  all zero, use the reference position).
C             (4)..> 0 => use DPARM(5)-DPARM(9) to describe the
C                  primary beam.
C                  <= 0 => use parameters appropriate to VLA.
C             (5)..The beam is described by the function:
C                     DPARM(5) + X*DPARM(6) + X*X*DPARM(7) +
C                        X*X*X*DPARM(8) + X*X*X*X*DPARM(9)
C                  where X is the square of the angular distance from
C                  the pointing position in arc minutes times the
C                  frequency in GHz.  For the VLA, these parms are
C                  0.9920378, 0.9956885e-3, 0.3814573e-5,
C                  -0.5311695e-8, and 0.3980963e-11, respectively.
C      RA(4)                    Pointing right ascension
C      DEC(4)                   Pointing declination
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER INNA*36, OUTNA*36, PRGNAM*6, RATYP(3)*4, DECTYP(3)*4,
     *   CTEMP*8
      INTEGER   IER, IERR, NPARM,I, J, INSL, OUTSL, LMAP, OMAP,
     *   DEPTH(5), OUTPTR, OUTVOL, INVOL, IARA, IADEC, IAFR, IAMIN,
     *   IDEPS(7), I1, I2, I3, I4, I5, I6, I7,
     *   IT1, IT2, IT3, IT4, IT5, IT6, IT7, INC, II
      LOGICAL   EQUAL, WASBLK
      HOLLERITH XRPARM(43), PTYPE
      REAL      RPARM(43), DATA(MAXIMG), MAPMAX, PBCORF, BLC(7), MAPMIN,
     *   TRC(7), PBPARM(7), XCOORD(6), PBLIMT, DOINV
      INTEGER   IC
      INCLUDE 'PBCOR.INC'
      INCLUDE 'PBCOR2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (XRPARM, RPARM)
      EQUIVALENCE (BLC(1), RPARM(9)),     (TRC(1), RPARM(16))
      EQUIVALENCE (PBPARM(1), RPARM(30)),  (XCOORD, RPARM(37))
      EQUIVALENCE (DOINV, RPARM(43))
      EQUIVALENCE (IDEPS(1), I1),         (IDEPS(2), I2)
      EQUIVALENCE (IDEPS(3), DEPTH(1))
      DATA PRGNAM /'PBCOR '/
      DATA LMAP, OMAP /17, 18/
      DATA RATYP, DECTYP /'RA  ','RA--','LL  ', 'DEC ','DEC-','MM  '/
C-----------------------------------------------------------------------
C                                       Start up task and get parms
      NPARM = 42
      IC = 0.0D0
      IER = 0
      TSKNAM = PRGNAM
      CALL TSKBEG (PRGNAM, NPARM, RPARM(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 950
         END IF
C                                       Store input and output name
      CALL CHR2H (2, 'MA', 1, PTYPE)
      RPARM(1) = NLUSER
      CALL H2WAWA (XRPARM(2), XRPARM(5), RPARM(7), PTYPE, RPARM(8),
     *   RPARM(1), INNA)
C                                       Output file name
      CALL H2WAWA (XRPARM(23), XRPARM(26), RPARM(28), PTYPE, RPARM(29),
     *   RPARM(1), OUTNA)
      DO 25 I = 1,10
         IBAD(I) = 0
 25      CONTINUE
      IF (PBPARM(1).LE.1.0E-3) PBPARM(1) = 0.023
      PBLIMT = PBPARM(1)
      CALL FILL (7, 0, LDEP)
C                                       Open input map
      CALL OPENCF (LMAP, INNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 950
         END IF
C                                       Get header
      CALL GETHDR (LMAP, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 950
         END IF
C                                       Find position axes
      IARA = 0
      IADEC = 0
      IAFR = 0
      INC = 2
      I1 = CATBLK(KIDIM)
      DO 40 I = 1,I1
         II = KHCTP + (I-1) * INC
         CALL H2CHR (4, 1, CATH(II), CTEMP)
         EQUAL = CTEMP(1:4) .EQ. 'FREQ'
         IF ((EQUAL) .AND. (IAFR.GT.0)) GO TO 45
         IF (EQUAL) IAFR = I
         DO 40 J = 1,3
            CALL H2CHR (4, 1, CATH(II), CTEMP)
            EQUAL = RATYP(J)(1:4) .EQ. CTEMP(1:4)
            IF ((EQUAL) .AND. (IARA.GT.0)) GO TO 45
            IF (EQUAL) IARA = I
            CALL H2CHR (4, 1, CATH(II), CTEMP)
            EQUAL = DECTYP(J)(1:4) .EQ. CTEMP(1:4)
            IF ((EQUAL) .AND. (IADEC.GT.0)) GO TO 45
            IF (EQUAL) IADEC = I
 40      CONTINUE
      IF ((IARA.GT.0) .AND. (IADEC.GT.0) .AND. (IAFR.GT.0)) GO TO 50
 45      WRITE (MSGTXT,1045)
         GO TO 950
C                                       Get pointing position
 50   IAMIN = MIN (IARA, IADEC)
      IAMIN = MIN (IAMIN, IAFR)
      EQUAL = (XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *   (XCOORD(3).LT.0.0)
      RAD0 = ABS (XCOORD(1)) + ABS (XCOORD(2))/60.0D0 +
     *   ABS (XCOORD(3))/3600.0D0
      IF (EQUAL) RAD0 = -RAD0
      RAD0 = RAD0 * 15.0D0
      EQUAL = (XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *   (XCOORD(6).LT.0.0)
      DECD0 = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *   ABS(XCOORD(6))/3600.0D0
      IF (EQUAL) DECD0 = -DECD0
      IF ((RAD0.EQ.0.0D0) .AND. (DECD0.EQ.0.0D0)) THEN
         RAD0 = CATD(KDORA)
         DECD0 = CATD(KDODE)
         IF ((RAD0.EQ.0.0D0) .AND. (DECD0.EQ.0.0D0)) THEN
            RAD0 = CATD(KDCRV+IARA-1)
            DECD0 = CATD(KDCRV+IADEC-1)
            END IF
         END IF
C                                       Set up window
      CALL WINDOW (FILTAB(PODIM,6), FILTAB(PONAX,6), BLC, TRC, IERR)
      IF (IERR.EQ.0) CALL MAPWIN (LMAP, BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 950
         END IF
      MAPMAX = -1.0E30
      MAPMIN =  1.0E30
      WASBLK = .FALSE.
      IT7 = TRC(7) - BLC(7) + 1.01
      IT6 = TRC(6) - BLC(6) + 1.01
      IT5 = TRC(5) - BLC(5) + 1.01
      IT4 = TRC(4) - BLC(4) + 1.01
      IT3 = TRC(3) - BLC(3) + 1.01
      IT2 = TRC(2) - BLC(2) + 1.01
      IT1 = TRC(1) - BLC(1) + 1.01
C                                       Create output map
      CALL COPY (256, CATBLK, CATOUT)
      CALL HDRWIN (BLC, TRC, CATOUT, IERR)
      CALL MAPCR (INNA, OUTNA, CATOUT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         GO TO 950
         END IF
C                                       Open output map
      CALL OPENCF (OMAP, OUTNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1070) IERR
         GO TO 950
         END IF
C                                       copy header keywords
      CALL FILKCP (LMAP, OMAP, IERR)
C                                       Loops
      DO 150 I7 = 1,IT7
         DEPTH(5) = I7
      DO 150 I6 = 1,IT6
         DEPTH(4) = I6
      DO 150 I5 = 1,IT5
         DEPTH(3) = I5
         IF (IAMIN.EQ.5) THEN
            I1 = 1
            I2 = 1
            DEPTH(2) = 1
            DEPTH(1) = 1
            CALL BMCORR (IDEPS, PBPARM, PBCORF, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
      DO 150 I4 = 1,IT4
         DEPTH(2) = I4
         IF (IAMIN.Eq.4) THEN
            I1 = 1
            I2 = 1
            DEPTH(1) = 1
            CALL BMCORR (IDEPS, PBPARM, PBCORF, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
      DO 150 I3 = 1,IT3
         DEPTH(1) = I3
         IF (IAMIN.EQ.3) THEN
            I1 = 1
            I2 = 1
            CALL BMCORR (IDEPS, PBPARM, PBCORF, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
         DO 150 I2 = 1,IT2
C                                       Read a map line
            CALL MAPIO ('READ', LMAP, DATA, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1120) J, IERR
               GO TO 950
               END IF
            IF (IAMIN.EQ.2) THEN
               I1 = 1
               CALL BMCORR (IDEPS, PBPARM, PBCORF, IERR)
               IF (IERR.NE.0) GO TO 940
               END IF
            DO 140 I1 = 1,IT1
               IF (IAMIN.EQ.1) CALL BMCORR (IDEPS, PBPARM, PBCORF,
     *            IERR)
               IF (IERR.NE.0) GO TO 940
               IF (DATA(I1).EQ.INDEF) THEN
                  WASBLK = .TRUE.
C                                       Are we too far out?
               ELSE
                  IF (PBCORF.LT.PBLIMT) THEN
                     IC = IC + 1
                     DATA(I1) = INDEF
                     WASBLK = .TRUE.
                  ELSE
                     IF (DOINV.GT.0.0) THEN
                        DATA(I1) = DATA(I1) * PBCORF
                     ELSE
                        DATA(I1) = DATA(I1) / PBCORF
                        END IF
                     IF (MAPMAX.LT.DATA(I1)) MAPMAX = DATA(I1)
                     IF (MAPMIN.GT.DATA(I1)) MAPMIN = DATA(I1)
                     END IF
                  END IF
 140           CONTINUE
C                                       Write an output line
            CALL MAPIO ('WRIT', OMAP, DATA, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1140) J, IERR
               GO TO 950
               END IF
 150     CONTINUE
      CATOR(KRBLK) = 0.0
      IF (WASBLK) CATOR(KRBLK) = INDEF
      CATOR(KRDMX) = MAPMAX
      CATOR(KRDMN) = MAPMIN
      PBLIMT = 100.0 * PBLIMT
      WRITE (MSGTXT,1150) IC, PBLIMT
      IF (IC.GT.0) CALL MSGWRT (5)
C                                       Get slot numbers
      CALL FILNUM (LMAP, OUTPTR, IERR)
      INVOL = FILTAB(POVOL,OUTPTR)
      INSL = FILTAB(POCAT,OUTPTR)
      CALL FILNUM (OMAP, OUTPTR, IERR)
      OUTVOL = FILTAB(POVOL,OUTPTR)
      OUTSL = FILTAB(POCAT,OUTPTR)
C                                       Close input map
      CALL FILCLS (LMAP)
C                                       Close output file
      CALL FILCLS (OMAP)
C                                       Add to HI file
      CALL PBHI (INNA, OUTNA, INSL, INVOL, OUTSL, OUTVOL, RPARM)
      GO TO 990
C-----------------------------------------------------------------------
C                                       Error return
 940  WRITE (MSGTXT,1940) I1, I2, DEPTH
 950  IER = 1
      CALL MSGWRT (8)
C                                       Normal return
 990  CALL TSKEND (IER)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('COULD NOT GET INPUT PARAMETERS. IERR=',I8)
 1025 FORMAT ('COULD NOT OPEN INPUT MAP.  IERR=',I8)
 1030 FORMAT ('COULD NOT GET INPUT HEADER.  IERR',I8)
 1045 FORMAT ('ONE OF RA, DEC, FREQ AXES IS MISSING - CANNOT DO',
     *   ' CORRECTION')
 1060 FORMAT ('COULD NOT GET MAP WINDOW.  IER=',I8)
 1065 FORMAT ('COULD NOT CREATE OUTPUT MAP.  IER=',I8)
 1070 FORMAT ('COULD NOT OPEN OUTPUT MAP.  IER=',I8)
 1120 FORMAT ('COULD NOT READ LINE #',I5,'  IER=',I8)
 1140 FORMAT ('COULD NOT WRITE LINE #',I5,'  IER=',I8)
 1150 FORMAT (I9,' Pixels blanked--outside',F5.1,'% primary beam level')
 1940 FORMAT ('POSITION ERROR AT PIXEL',3I5,4I4)
      END
      SUBROUTINE BMCORR (ID, PBPARM, PBCORF, IERR)
C-----------------------------------------------------------------------
C   BMCORR finds the frequency, RA, dec of the current sample in the
C   output image and uses that to find the beam correction
C   Inputs:
C      ID       I(7)   Current pixel number
C   In/out:
C      PBPARM   R(7)   Beam parameters, PBPARM(1) cutoff
C   Output:
C      PBCORF   R      Beam value
C      IERR     I      0 => okay, else illegal position
C-----------------------------------------------------------------------
      INTEGER   ID(7), IERR
      REAL      PBPARM(*), PBCORF
C
      DOUBLE PRECISION DX, DY, DT, X, Y, LAMBDA
      LOGICAL   F, OUTSID
      CHARACTER ARRAY*8
      INCLUDE 'PBCOR.INC'
      INCLUDE 'PBCOR2.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LAMBDA
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
C                                       Init common each plane
      IF ((ID(1).EQ.1) .AND. (ID(2).EQ.1)) THEN
         CALL FILL (7, 0, LDEP)
         CALL COPY (256, CATOUT, CATBLK)
         LOCNUM = 1
         CALL SETLOC (ID(3), F)
C                                       Pointing pixel 1st time in
         IF ((ID(3).EQ.1) .AND. (ID(4).EQ.1) .AND. (ID(5).EQ.1) .AND.
     *      (ID(6).EQ.1) .AND. (ID(7).EQ.1)) THEN
            XINC = CATOR(KRCIC+KLOCL(LOCNUM)) * COND2R
            YINC = CATOR(KRCIC+KLOCM(LOCNUM)) * COND2R
            IERR = 2
            IF ((XINC.EQ.0.0) .OR. (YINC.EQ.0.0)) GO TO 999
            COSR = COS (ROT(LOCNUM) * COND2R)
            SINR = SGNROT(LOCNUM) * SIN (ROT(LOCNUM) * COND2R)
            RREFR = CATOD(KDCRV+KLOCL(LOCNUM)) * COND2R
            DREFR = CATOD(KDCRV+KLOCM(LOCNUM)) * COND2R
            RPTR = RAD0 * COND2R
            DPTR = DECD0 * COND2R
            DPTRS = SIN (DPTR)
            DPTRC = COS (DPTR)
            CALL DIRCOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RREFR, DREFR,
     *         RPTR, DPTR, X, Y, IERR)
            IF (IERR.NE.0) GO TO 999
            YPIX0 = (Y * COSR - X * SINR) / YINC +
     *         CATOR(KRCRP+KLOCM(LOCNUM))
            XPIX0 = (X * COSR + Y * SINR) / XINC +
     *         CATOR(KRCRP+KLOCL(LOCNUM))
            END IF
         END IF
C                                       Frequency
      IF (ID(KLOCF(LOCNUM)+1).NE.LDEP(KLOCF(LOCNUM)+1)) THEN
         FRL0 = (CATOD(KDCRV+KLOCF(LOCNUM)) + (ID(KLOCF(LOCNUM)+1) -
     *      CATOR(KRCRP+KLOCF(LOCNUM))) * CATOR(KRCIC+KLOCF(LOCNUM)))
         LAMBDA = VELITE / FRL0
         END IF
C                                       Position term
      IF ((ID(KLOCL(LOCNUM)+1).NE.LDEP(KLOCL(LOCNUM)+1)) .OR.
     *   (ID(KLOCM(LOCNUM)+1).NE.LDEP(KLOCM(LOCNUM)+1))) THEN
C                                       Accurate method
C                                       Offset, rotation to position
         DX = (ID(KLOCL(LOCNUM)+1) - CATOR(KRCRP+KLOCL(LOCNUM))) * XINC
         DY = (ID(KLOCM(LOCNUM)+1) - CATOR(KRCRP+KLOCM(LOCNUM))) * YINC
         DT = DX * COSR - DY * SINR
         DY = DY * COSR + DX * SINR
         DX = DT
         CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RREFR, DREFR,
     *      DX, DY, X, Y, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       get actual angle
         DT = DPTRS * SIN(Y) + DPTRC * COS(Y) * COS(RPTR-X)
         IF (DT.GT.1.0D0) DT = 1.0D0
         IF (DT.LT.-1.0D0) DT = -1.0D0
         POL0 = RAD2DG * ACOS (DT)
         END IF
C                                       Correction factor
      CALL PBCALC (POL0, LAMBDA, ARRAY, PBPARM(2), PBCORF, OUTSID)
      IF (OUTSID) PBCORF = 0.0
      CALL COPY (7, ID, LDEP)
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE PBHI (INNA, CTNA, INSL, IVOL, OUTSL, OVOL, RPARM)
C-----------------------------------------------------------------------
C   PBHI creates and writes the HI file for the task PBCOR.
C   Inputs:
C      INNA     C*36     Input map name, etc.
C      CTNA     C*36     Output map name, etc.
C      INSL     I        Slot number for input map
C      OUTSL    I        Slot number for output map
C      RPARM    R(*)     Input parameters
C      CATOUT   I(256)   Output map header
C-----------------------------------------------------------------------
      CHARACTER INNA*36, CTNA*36
      INTEGER   INSL, IVOL, OUTSL, OVOL, IERR
      REAL      RPARM(*)
C
      CHARACTER HILINE*72, CHRASN*1, CHDCSN*1, NOTTYP(1)*2
      INTEGER   NHISTF, LHIN, LHOUT, I, IBUFF1(1024), IBUFF2(1024),
     *   INSEQ, BLC(7), IROUND, TRC(7), RA(2), DEC(2), NONOT
      REAL      RAS, DECS, RTEMP
      HOLLERITH HTEMP
      LOGICAL   T
      INCLUDE 'PBCOR.INC'
      INCLUDE 'PBCOR2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DPBCALC.INC'
      DATA NHISTF, LHIN, LHOUT /2, 27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /1, 'CC'/
C-----------------------------------------------------------------------
C                                       Initialize HI
      CALL HIINIT (NHISTF)
C                                       Create and open output HI file
      CALL HISCOP (LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL, CATOUT,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 60
         END IF
C                                       Add new HI entries
      CALL CHR2H (4, INNA(21:), 1, HTEMP)
      RTEMP = HTEMP
      INSEQ = IROUND(RTEMP)
C                                          Input name
      CALL HENCO1 (TSKNAM, INNA(1:), INNA(13:), INSEQ, IVOL, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                          Output name
      CALL CHR2H (4, CTNA(21:), 1, HTEMP)
      RTEMP = HTEMP
      INSEQ = IROUND(RTEMP)
      CALL HENCOO (TSKNAM, CTNA(1:), CTNA(13:), INSEQ, OVOL, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                          BLC and TRC
      DO 25 I = 1,7
         BLC(I) = RPARM(8+I) + 0.01
         TRC(I) = RPARM(15+I) + 0.01
 25      CONTINUE
      WRITE (HILINE,1025) TSKNAM, BLC, TRC
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                          Pointing RA and DEC
      CALL COORDD (1, RAD0, CHRASN, RA, RAS)
      CALL COORDD (2, DECD0, CHDCSN, DEC, DECS)
      WRITE (HILINE,1026) TSKNAM, CHRASN, RA, RAS, CHDCSN, DEC, DECS
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       cutoff level
      WRITE (HILINE,1027) TSKNAM, RPARM(30)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       cutoff level
      WRITE (HILINE,1028) TSKNAM, RPARM(43)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       beam shape
      WRITE (HILINE,1030) TSKNAM, 1.0
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
      WRITE (HILINE,1031) TSKNAM, BMPARM(1)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
      WRITE (HILINE,1032) TSKNAM, BMPARM(2)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
      WRITE (HILINE,1033) TSKNAM, BMPARM(3)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
      IF (BMPARM(4).NE.0.0D0) THEN
         WRITE (HILINE,1034) TSKNAM, BMPARM(4)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      IF (BMPARM(5).NE.0.0D0) THEN
         WRITE (HILINE,1035) TSKNAM, BMPARM(5)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      WRITE (HILINE,1036) TSKNAM
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       new EVLA beams
      IF (PBFRQ2.GT.0.0D0) THEN
         WRITE (HILINE,1040) TSKNAM, PBFRQ1
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1041) TSKNAM, PBFRQ2
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1030) TSKNAM, 1.0
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1031) TSKNAM, BMPRM2(1)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1032) TSKNAM, BMPRM2(2)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1033) TSKNAM, BMPRM2(3)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         IF (BMPRM2(4).NE.0.0D0) THEN
            WRITE (HILINE,1034) TSKNAM, BMPRM2(4)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 60
            END IF
         IF (BMPRM2(5).NE.0.0D0) THEN
            WRITE (HILINE,1035) TSKNAM, BMPRM2(5)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 60
            END IF
         WRITE (HILINE,1036) TSKNAM
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      GO TO 70
C                                       Error return
 60   WRITE (MSGTXT,1060) IERR
      CALL MSGWRT (8)
C                                       Close HI file
 70   CALL HICLOS (LHOUT, T, IBUFF2, IERR)
C                                       Copy tables
C                                       omit CC files - not true after
      CALL ALLTAB (NONOT, NOTTYP, LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL,
     *   CATOUT, IBUFF1, IBUFF2, IERR)
      IF (IERR.GE.1) THEN
         WRITE (MSGTXT,1070) IERR
         CALL MSGWRT (6)
         END IF
C
999   RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CANNOT COPY HI FILE.  IER=',I8)
 1025 FORMAT (A6,'BLC=',2(I4,','),I4,2(',',I3),2(',',I2),' TRC=',
     *   2(I4,','),I4,2(',',I3),2(',',I2))
 1026 FORMAT (A6,'PRA= ',A1,I2.2,I3.2,F7.3,'  PDec= ',A1,I2.2,I3.2,
     *   F6.2)
 1027 FORMAT (A6,'Beamcut=',F9.5,3X,'/Clip outside this beam value')
 1028 FORMAT (A6,'DOINVERS =',F4.1,3X,'/ > 0 multiply by beam')
 1030 FORMAT (A6,'/ Beam = ',1PE13.6,' +',5X,'/ Beam  model')
 1031 FORMAT (A6,'/',8X,1PE13.6,' * X +')
 1032 FORMAT (A6,'/',8X,1PE13.6,' * X*X +')
 1033 FORMAT (A6,'/',8X,1PE13.6,' * X*X*X +')
 1034 FORMAT (A6,'/',8X,1PE13.6,' * X*X*X*X')
 1035 FORMAT (A6,'/',8X,1PE13.6,' * X*X*X*X*X')
 1036 FORMAT (A6,'/ Where X = [Angle(arcmin) * f(GHz)]**2')
 1040 FORMAT (A6,'/ above from EVLA 2016 model interpolated between',
     *   F8.3,' GHz')
 1041 FORMAT (A6,'/ and',F8.3,' GHz parameters below')
 1060 FORMAT ('CANNOT ADD LINES TO HI FILE.  IER=',I8)
 1070 FORMAT ('ERROR ',I3,' COPYING TABLES')
      END

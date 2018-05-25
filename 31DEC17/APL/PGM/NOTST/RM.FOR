LOCAL INCLUDE 'RM.INC'
      HOLLERITH XINAME(3), XINCLS(2), XINAM2(3), XICLS2(2), XONAME(3),
     *   XOCLS(2)
      REAL      XISEQ, XIDISK, XISEQ2, XDISK2, XBLC(7), XTRC(7), XOSEQ,
     *   XODISK, APARM(10), SCALR1, XUSER
      COMMON /INPARM/ XUSER, XINAME, XINCLS, XISEQ, XIDISK, XINAM2,
     *   XICLS2, XISEQ2, XDISK2, XBLC, XTRC,XONAME, XOCLS, XOSEQ,
     *   XODISK, APARM, SCALR1
LOCAL END
      PROGRAM RM
C-----------------------------------------------------------------------
C! Calculates rotation measure and intrinsic magnetic field from cube.
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008-2009, 2012, 2015
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
C   RM calculates the rotation measure and intrinsic magnetic field
C   from an input data cube whose first axis is frequency.  The current
C   version works with 3 or 4 input frequencies.
C   Inputs:
C      INNAME                   Image name (name)
C      INCLASS                  Image name (class)
C      INSEQ                    Image name (seq. #)
C      INDISK                   Disk unit # of image
C      BLC(7)                   Bottom Left corner to calculate RM
C      TRC(7)                   Top right corner
C      OUTNAME                  Output image name (name)
C      OUTCLASS(2)              Unused
C      OUTSEQ                   Output image name (seq. #)
C      OUTDISK                  Output image disk
C      APARM(10)                Information for solution:
C        (1): Begin channel
C        (2): end channel in fit
C        (3): The initial guess for the rotation measure. Use
C             the integrated values if nothing else is known.
C             UNITS are RADIANS per METER squared !
C        (4)  Maximum abs value of RM expected
C        (5): Solution type. 0 => Unweighted fit
C             1 =>  Weight fit by errors.
C        (6): Blanking type. 0 => No blanking
C             1 => Blank both output maps if corr. coeff. < APARM(7)
C             2 => Blank both maps only if sigma of RM exceeds
C                  APARM(7) (rad/m.m)
C             3 => Blank both maps only if sigma of B > APARM(7)
C                  (degrees)
C             4 => Blank both maps if rms dev. per point from best
C                  fit line exceeds APARM(7) (degrees)
C             5 => Blank both output maps if in input error of any
C                  input map value exceeds APARM(7).
C                  THIS IS THE RECOMMENDED WAY.
C        (7): The blanking level.  See APARM(6).
C      SCALR1                   Output value  (<= 0) or sigma maps (>0)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER INNA*36, IN2NA*36, OUT2NA*36, OUTNA*36, PRGNAM*6,
     *   CHTMP*8, STOKES*4, ROTM*8, BFIELD*8, RUNITS*8, FAXIS*8
      HOLLERITH MAP, HCLASS(2), CATIH(256)
      INTEGER   IERR, NPARM, NFREQ, NPTS(2), NX, NY, I, J, INSL,
     *   OUTSL, OMAP2, K, OUT2SL, IBLNK, CATIN(256), LMAP, OMAP, IER,
     *   NPL, NMAP, ip
      LOGICAL   WASBLK, SOLTYP, OUTERR
      REAL      DATA(20), RTM(MAXIMG), BPA(MAXIMG), ROTMS, EPA, Q0,
     *   CATIR(256), BLC(7), TRC(7), RM0, ROTSIG, PASIG, CORR, BLKLEV,
     *   PHRMS, RTMAX, RTMIN, BPAMAX, BPAMIN, WAVSQ(MAXIMG), RMMAX
      DOUBLE PRECISION CATID(128), FREQ(MAXIMG)
      INCLUDE 'RM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATIN, CATIR, CATID, CATIH)
      DATA PRGNAM /'RM    '/
      DATA STOKES /'STOK'/
      DATA LMAP, OMAP, OMAP2, NMAP /17, 18, 19, 20/
      DATA ROTM, BFIELD /'ROTMES  ', 'BFIELD  '/
      DATA RUNITS /'RAD/M/M '/
C-----------------------------------------------------------------------
C                                       Start up task and get parms
      NPARM = 46
      IER = 8
      CALL TSKBEG (PRGNAM, NPARM, XINAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING INPUT ADVERBS'
         GO TO 950
         END IF
C                                       Store input and output name
      CALL CHR2H (4, 'MA  ', 1, MAP)
      XUSER = NLUSER
      CALL H2WAWA (XINAME, XINCLS, XISEQ, MAP, XIDISK, XUSER, INNA)
      CALL H2WAWA (XINAM2, XICLS2, XISEQ2, MAP, XDISK2, XUSER, IN2NA)
      CALL CHR2H (6, ROTM, 1, HCLASS)
      CALL H2WAWA (XONAME, HCLASS, XOSEQ, MAP, XODISK, XUSER, OUTNA)
      CALL CHR2H (6, BFIELD, 1, HCLASS)
      CALL H2WAWA (XONAME, HCLASS, XOSEQ, MAP, XODISK, XUSER, OUT2NA)
      DO 10 I = 1,10
         IBAD(I) = 0
 10      CONTINUE
C                                       Open input map
      CALL OPENCF (LMAP, INNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INPUT IMAGE'
         GO TO 950
         END IF
C                                       Get header
      CALL GETHDR (LMAP, CATIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING INPUT IMAGE HEADER'
         GO TO 950
         END IF
C                                       Set up window
      IF (XBLC(1).EQ.0.0) XBLC(1) = 1.0
      IF (XBLC(2).EQ.0.0) XBLC(2) = 1.0
      IF (XBLC(3).EQ.0.0) XBLC(3) = 1.0
      IF (XTRC(1).EQ.0.0) XTRC(1) = CATIN(KINAX+1)
      IF (XTRC(2).EQ.0.0) XTRC(2) = CATIN(KINAX+2)
      BLC(1) = 1
      CALL RCOPY (6, XBLC(1), BLC(2))
      NFREQ = CATIN(KINAX)
      NPTS(1) = APARM(1) + 0.01
      NPTS(1) = MAX (1, NPTS(1))
      NPTS(2) = APARM(2) + 0.01
      IF (NPTS(2).LT.NPTS(1)) NPTS(2) = NFREQ
      IF (NPTS(2)-NPTS(1).LT.2) THEN
         NPTS(1) = 1
         NPTS(2) = NFREQ
         END IF
      IF (NFREQ.LT.3) THEN
         IERR = 10
         MSGTXT = 'TOO FEW POINTS ON f AXIS FOR RM FIT'
         GO TO 950
         END IF
      TRC(1) = CATIN(KINAX)
      CALL RCOPY (6, XTRC(1), TRC(2))
      CALL WINDOW (FILTAB(PODIM,6), FILTAB(PONAX,6), BLC, TRC, IERR)
      NX = TRC(2) - BLC(2) + 1
      NY = TRC(3) - BLC(3) + 1
      CALL MAPWIN (LMAP, BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'SETTING INPUT IMAGE WINDOW'
         GO TO 950
         END IF
      CALL RCOPY (6, BLC(2), XBLC(1))
      CALL RCOPY (6, TRC(2), XTRC(1))
      SOLTYP = APARM(5).GT.0.0
      IF (.NOT.SOLTYP) THEN
         IN2NA = ' '
C                                       Open input noise map
      ELSE
         CALL OPENCF (NMAP, IN2NA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING INPUT NOISE IMAGE'
            GO TO 950
            END IF
         CALL MAPWIN (NMAP, BLC, TRC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'SETTING INPUT NOISE IMAGE WINDOW'
            GO TO 950
            END IF
         END IF
C                                       Get frequencies, and convert
      CALL H2CHR (8, 1, CATH(KHCTP), FAXIS)
      IF (FAXIS.NE.'FQID') THEN
C                                       is there a freq axis
         IF (FAXIS.NE.'FREQ') THEN
            MSGTXT = 'NEITHER FQID NOR FREQ AXIS AS FIRST AXIS'
            GO TO 950
            END IF
         DO 20 I = 1,NFREQ
            FREQ(I) = CATD(KDCRV) + (I-CATR(KRCRP)) * CATR(KRCIC)
 20         CONTINUE
C                                       get FQ table
      ELSE
         CALL GETFQV (NFREQ, FREQ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'GETTING FQ FREQUENCIES'
            GO TO 950
            END IF
         END IF
C                                       to wavelength squared in MKS
      DO 25 I = NPTS(1),NPTS(2)
         IF (FREQ(I).LT.1.D5) THEN
            IERR = 10
            WRITE (MSGTXT,1020) I, FREQ(I)
            GO TO 950
         ELSE
            WAVSQ(I) = (VELITE/FREQ(I))**2
            END IF
 25      CONTINUE
C                                       Read solution type
      BLKLEV = APARM(7)
      IBLNK = APARM(6) + 0.01
      IBLNK = MAX (0, MIN (5, IBLNK))
      NPL = (TRC(7)-BLC(7)+1.) * (TRC(6)-BLC(6)+1.) *
     *   (TRC(5)-BLC(5)+1.) * (TRC(4)-BLC(4)+1.)
      IF ((.NOT.SOLTYP) .AND. (IBLNK.EQ.5)) IBLNK = 0
      APARM(6) = IBLNK
      OUTERR = SCALR1.GT.0.0
      WASBLK = .FALSE.
      RTMAX = -1.E20
      RTMIN = 1.E20
      BPAMAX = -1.E20
      BPAMIN = 1.E20
C                                       Get RM guess, convert
C                                        to deg./sq. meter
      RM0 = APARM(3) * 57.2957795
      RMMAX = APARM(4) * 57.2957795
C                                       Create output map
      CALL COPY (256, CATIN, CATBLK)
      CALL HDRWIN (BLC, TRC, CATBLK, IERR)
C                                       Swap axes to 2-dim image
      CALL SWAPAX (CATBLK, CATH, CATR, CATD, 1, 2, IERR)
      CALL SWAPAX (CATBLK, CATH, CATR, CATD, 2, 3, IERR)
      CATBLK(KINAX+2) = 1
      CALL MAPCR (INNA, OUT2NA, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING 2ND OUTPUT IMAGE'
         GO TO 950
         END IF
      CALL CHR2H (8, RUNITS, 1, CATH(KHBUN))
      DO 30 I = 3,6
         CALL H2CHR (4, 1, CATH(KHCTP+I*2), CHTMP)
         IF (STOKES(:4) .EQ. CHTMP(:4)) THEN
            CATD(KDCRV+I) = 10.0D0
            GO TO 35
            END IF
 30      CONTINUE
 35   CALL MAPCR (INNA, OUTNA, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING 1ST OUTPUT IMAGE'
         GO TO 950
         END IF
C                                       Open output map
      CALL OPENCF (OMAP, OUTNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING 1ST OUTPUT IMAGE'
         GO TO 950
         END IF
      CALL OPENCF (OMAP2, OUT2NA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEING 2ND OUTPUT IMAGE'
         GO TO 950
         END IF
C                                       Read a map Line
      DO 200 IP = 1,NPL
      DO 190 J = 1,NY
         DO 180 I = 1,NX
            CALL MAPIO ('READ', LMAP, DATA, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, 'READING INPUT IMAGE', J
               GO TO 950
               END IF
            IF (SOLTYP) THEN
               CALL MAPIO ('READ', NMAP, DATA(1+NFREQ), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR, 'READING INPUT IMAGE', J
                  GO TO 950
                  END IF
               END IF
C                                       If any input pixel blanked,
C                                       blank output
            DO 120 K= NPTS(1),NPTS(2)
               IF (DATA(K).EQ.FBLANK) GO TO 170
 120           CONTINUE
C                                       If blanking on input sigmas,
C                                       test on input sigmas
            IF (IBLNK.GT.4) THEN
               DO 125 K = NPTS(1),NPTS(2)
                  IF (DATA(K+NFREQ).GT.BLKLEV) GO TO 170
 125              CONTINUE
               END IF
C                                       Calculate Rot. Meas. and errors
            IF (APARM(4).LE.0.0) THEN
               CORR = 2. * BLKLEV + 1.
               CALL ROTFIT (DATA, SOLTYP, WAVSQ, NFREQ, NPTS, RM0,
     *            ROTMS, EPA, ROTSIG, PASIG, PHRMS)
            ELSE
               CALL ROTFZH (DATA, SOLTYP, WAVSQ, NFREQ, NPTS, RM0,
     *            RMMAX, ROTMS, EPA, ROTSIG, PASIG, CORR, PHRMS, Q0)
               END IF
C                                       Convert back to socially
C                                       acceptable units
            ROTMS = ROTMS / 57.2957795
            ROTSIG = ROTSIG / 57.2957795
C                                       Output map values or map errors?
            IF (OUTERR) THEN
               RTM(I) = ROTSIG
               BPA(I) = PASIG
               GO TO 160
               END IF
C                                       Blank by correlation coefficient
            IF ((IBLNK.EQ.1) .AND.(ABS(CORR).LE.BLKLEV))  GO TO 170
C                                       Blank by sigma of R.M.
            IF ((IBLNK.EQ.2) .AND. (ROTSIG.GE.BLKLEV)) GO TO 170
C                                       Blank by sigma intrinsic B field
            IF ((IBLNK.EQ.3) .AND. (PASIG.GE.BLKLEV)) GO TO 170
C                                       Blank by rms of phase fit
            IF ((IBLNK.EQ.4) .AND. (PHRMS.GT.BLKLEV)) GO TO 170
C                                       not blanked
            RTM(I) = ROTMS
            BPA(I) = EPA + 90.
 140        IF (BPA(I).LT.0.) THEN
               BPA(I) = BPA(I) + 180.
               GO TO 140
               END IF
 150        IF (BPA(I).GT.180.) THEN
               BPA(I) = BPA(I) - 180.
               GO TO 150
               END IF
 160        BPAMAX = MAX (BPAMAX, BPA(I))
            BPAMIN = MIN (BPAMIN, BPA(I))
            RTMAX = MAX (RTMAX, RTM(I))
            RTMIN = MIN (RTMIN, RTM(I))
            GO TO 180
C                                       blank the result
 170        RTM(I) = FBLANK
            BPA(I) = FBLANK
            WASBLK = .TRUE.
 180        CONTINUE
C                                       Write an output Line
         CALL MAPIO ('WRIT', OMAP, RTM, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRITING 1ST OUTPUT IMAGE', J
            GO TO 950
            END IF
         CALL MAPIO ('WRIT', OMAP2, BPA ,IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRITING 2ND OUTPUT IMAGE', J
            GO TO 950
            END IF
 190     CONTINUE
 200     CONTINUE
C                                       Get slot numbers
      INSL = FILTAB(POCAT,6)
C                                       Close input map
      CALL FILCLS (LMAP)
C                                       Close output files
      CALL FILCLS (OMAP)
      CALL FILCLS (OMAP2)
C                                       Add to HI fiLe
      CALL OPENCF (OMAP, OUTNA, IERR)
      OUTSL = FILTAB(POCAT,6)
      CALL GETHDR (OMAP, CATBLK, IERR)
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CATR(KRDMX) = RTMAX
      CATR(KRDMN) = RTMIN
      CALL FILCLS (OMAP)
      IER = 0
      CALL RMHI (INNA, OUTNA, INSL, OUTSL, NPTS, FREQ, CATBLK)
      CALL OPENCF (OMAP2, OUT2NA, IERR)
      OUT2SL = FILTAB(POCAT,6)
      CALL GETHDR (OMAP2, CATBLK, IERR)
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CATR(KRDMX) = BPAMAX
      CATR(KRDMN) = BPAMIN
      CALL FILCLS (OMAP2)
      CALL RMHI (INNA, OUT2NA, INSL, OUT2SL, NPTS, FREQ, CATBLK)
      GO TO 990
C                                       Error return
 950  CALL MSGWRT (8)
C                                       Normal return
 990  CALL TSKEND (IER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,1X,A)
 1020 FORMAT ('FREQUENCY',I4,' ILLEGAL',1PD12.4)
 1100 FORMAT ('ERROR',I4,1X,A,' ROW',I6)
      END
      SUBROUTINE RMHI (INNA, CTNA, INSL, OUTSL, NPTS, FREQ, CATBLK)
C-----------------------------------------------------------------------
C   RMHI creates and Writes the HI fiLe for the task RM.
C   Inputs:
C      INNA     C*36     Input map name, etc.
C      CTNA     C*36     Output map name, etc.
C      INSL     I        Slot number for input map
C      OUTSL    I        Slot number for output map
C      FREQ     D(*)     Frequencies in Hz
C      CATBLK   I(256)   Output map header
C-----------------------------------------------------------------------
      CHARACTER INNA*36, CTNA*36
      INTEGER   INSL, OUTSL, NPTS(2), CATBLK(256)
      DOUBLE PRECISION FREQ(*)
C
      CHARACTER HILINE*72, INAME*12, ICLASS*6, PTYPE*2, ONAME*12,
     *   OCLASS*6
      INTEGER   IERR, NHISTF, LHIN, LHOUT, I, J, IBUFF1(256), USID,
     *   IBUFF2(256), BLC(6), TRC(6), ISEQ, OSEQ, IVOL, OVOL
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'RM.INC'
      DATA NHISTF, LHIN, LHOUT /2,27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL WAWA2A (INNA, INAME, ICLASS, ISEQ, PTYPE, IVOL, USID)
      CALL WAWA2A (CTNA, ONAME, OCLASS, OSEQ, PTYPE, OVOL, USID)
C                                       copy some keywords
      CALL KEYPCP (IVOL, INSL, OVOL, OUTSL, 0, ' ', IERR)
C                                       Initialize HI
      CALL HIINIT (NHISTF)
C                                       Create and open output HI fiLe
      CALL HISCOP (LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL, CATBLK,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 65
         END IF
C                                       Add new HI entries
C                                          Input name
      CALL HENCO1 (TSKNAM, INAME, ICLASS, ISEQ, IVOL, LHOUT, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 60
C                                          Output name
      CALL HENCOO (TSKNAM, ONAME, OCLASS, OSEQ, OVOL, LHOUT, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 60
C                                       Type output
      IF (SCALR1.LE.0.0) THEN
         WRITE (HILINE,1020) TSKNAM
      ELSE
         WRITE (HILINE,1021) TSKNAM
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       BLC and TRC
      DO 25 I = 1,6
         BLC(I) = XBLC(I) + 0.01
         TRC(I) = XTRC(I) + 0.01
 25      CONTINUE
      WRITE (HILINE,1025) TSKNAM, BLC, TRC
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       Frequencies
      WRITE (HILINE,1026) TSKNAM, NPTS
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
      DO 30 I = 1,J
         WRITE (HILINE,1027) TSKNAM, I, FREQ(I)/1.D6
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
 30      CONTINUE
C                                       Initial guess
      WRITE (HILINE,1030) TSKNAM, APARM(3)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       Max expected
      IF (APARM(4).GT.0.0) THEN
         WRITE (HILINE,1031) TSKNAM, APARM(4)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
C                                       Weighted?
      IF (APARM(5).GT.0.0)  THEN
         WRITE (HILINE,1035) TSKNAM
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
C                                       Blanking
      I = APARM(6) + 0.01
      IF ((I.GT.0) .AND. (I.LE.5)) THEN
         IF (I.EQ.1) WRITE (HILINE,1041) TSKNAM, APARM(7)
         IF (I.EQ.2) WRITE (HILINE,1042) TSKNAM, APARM(7)
         IF (I.EQ.3) WRITE (HILINE,1043) TSKNAM, APARM(7)
         IF (I.EQ.4) WRITE (HILINE,1044) TSKNAM, APARM(7)
         IF (I.EQ.5) WRITE (HILINE,1045) TSKNAM, APARM(7)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      GO TO 70
C                                       Error
 60   WRITE (MSGTXT,1060) IERR
 65   CALL MSGWRT (8)
C                                       Close HI fiLe
 70   CALL HICLOS (LHOUT, T, IBUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CANNOT COPY HI FILE.  IER=',I8)
 1020 FORMAT (A6,'/ Output images of rotation measure and bfield')
 1021 FORMAT (A6,'/ Output images of uncertainties in rotation',
     *   ' measure and bfield')
 1025 FORMAT (A6,'BLC=',2(I5,','),I4,3(',',I3),'  TRC=',2(I5,','),
     *   I4,3(',',I3))
 1026 FORMAT (A6,'BFREQ=',I3,'  EFREQ=',I3,3X,'/ Frequency channels',
     *   ' in fit')
 1027 FORMAT (A6,'FREQ(',I3,')=',1PE13.5,' / MHz')
 1030 FORMAT (A6,'RM0 =',F13.4,' / Rad/m/m  initial guess')
 1031 FORMAT (A6,'RMmax =',F13.4,' / Rad/m/m max expected, fzhou',
     *   ' method')
 1035 FORMAT (A6,'/ Fit weighted by error images')
 1041 FORMAT (A6,'/ blanked if corr. coeff. <',1PE13.5)
 1042 FORMAT (A6,'/ blanked if sigma(RM) >',1PE13.5,' Rad/m/m')
 1043 FORMAT (A6,'/ blanked if sigma(B) >',1PE13.5,' degrees')
 1044 FORMAT (A6,'/ blanked if RMS >',1PE13.5,' degrees')
 1045 FORMAT (A6,'/ blanked if input RMS >',1PE13.5,' degrees')
 1060 FORMAT ('CANNOT ADD LINES TO HI FILE.  IER=',I8)
      END
      SUBROUTINE ROTFIT (DATA, SOLTYP, WAVSQ, NFREQ, NPTS, RM0, ROTMS,
     *   EPA, SIGROT, SIGPA, PHRMS)
C-----------------------------------------------------------------------
C   ROTFIT sets up the data for the least squares fit.  Its most
C   important task is to discover and remove ambiguities from the data.
C   Inputs:
C      DATA     R(*)   Input data, the position angles at various
C                      frequencies measured at the same RA and Dec.
C      SOLTYP   L      Solution type: T => weighted
C      WAVSQ    R(*)   Array of squared wavelengths.
C      RM0      R      The initial guess of the rot. meas.
C      NFREQ    R      The number of frequencies.
C   Outputs:
C      ROTMS    R      The rotation measure.
C      EPA      R      Intrinsic value of electric vector.
C      SIGROT   R      Error in the rotation measure
C      SIGPA    R      Error in the position angle
C-----------------------------------------------------------------------
      REAL      DATA(*), WAVSQ(*), RM0, ROTMS, EPA, SIGROT, SIGPA, PHRMS
      LOGICAL   SOLTYP
      INTEGER   NFREQ, NPTS(2)
C
      INTEGER   I, J, LPTS(2)
      REAL      PHSUM, DELRM0, DIF
C-----------------------------------------------------------------------
C                                       Correct the input angles by the
C                                       initial r.m. guess
      DO 20 I = 1,NFREQ
         DATA(I) = DATA(I) - RM0 * WAVSQ(I)
 20      CONTINUE
C                                       We assume no ambiguities between
C                                       the first two frequencies
      DIF = DATA(NPTS(1)) - DATA(NPTS(1)+1)
      IF (DIF.GT.90) DATA(NPTS(1)+1) = DATA(NPTS(1)+1) + 180.
      IF (DIF.LT.-90) DATA(NPTS(1)+1) = DATA(NPTS(1)+1) - 180.
      DELRM0 = (DATA(NPTS(1)+1) - DATA(NPTS(1))) /
     *   (WAVSQ(NPTS(1)+1) - WAVSQ(NPTS(1)))
C                                       Now remove additional term
      DO 25 I = 1,NFREQ
         DATA(I) = DATA(I) - DELRM0 * WAVSQ(I)
 25      CONTINUE
C                                       Now remove amgiguities between
C                                       the second and third frequencies
      DIF = DATA(NPTS(1)+1) - DATA(NPTS(1)+2)
      IF (DIF.GT.450) DATA(NPTS(1)+2) = DATA(NPTS(1)+2) + 180.
      IF (DIF.GT.270) DATA(NPTS(1)+2) = DATA(NPTS(1)+2) + 180
      IF (DIF.GT.90.) DATA(NPTS(1)+2) = DATA(NPTS(1)+2) + 180.
      IF (DIF.LT.-450) DATA(NPTS(1)+2) = DATA(NPTS(1)+2) - 180.
      IF (DIF.LT.-270) DATA(NPTS(1)+2) = DATA(NPTS(1)+2) - 180.
      IF (DIF.LT.-90.) DATA(NPTS(1)+2) = DATA(NPTS(1)+2) - 180.
C                                       Retrend the DATA to get a
C                                       meaningful corr. coeff.
      DO 30 J = 1,NFREQ
         DATA(J) = DATA(J) + (RM0 + DELRM0) * WAVSQ(J)
 30      CONTINUE
C                                       Do a least squares fit on the
C                                       first 3 points
      LPTS(1) = NPTS(1)
      LPTS(2) = NPTS(1) + 2
      CALL RMSFIT (DATA, SOLTYP, WAVSQ, NFREQ, LPTS, ROTMS, EPA, SIGROT,
     *    SIGPA)
C                                       If we are to fit to 3 points
C                                       only, we are finished.
      IF (NPTS(2)-NPTS(1)+1.LE.3) GO TO 100
C                                       We hope we are close to the
C                                       correct answer. Adjust the data
C                                       by the latest RM estimate
      DO 35 I = 1,NFREQ
         DATA(I) = DATA(I) - ROTMS * WAVSQ(I)
 35      CONTINUE
C                                       Now we correct for those awful
C                                       ambiguities
      DO 40 I = 2,NFREQ
         DIF = DATA(I-1) - DATA(I)
         IF (DIF.GT.450) DATA(I) = DATA(I) + 180.
         IF (DIF.GT.270) DATA(I) = DATA(I) + 180.
         IF (DIF.GT. 90) DATA(I) = DATA(I) + 180.
         IF (DIF.LT.-450) DATA(I)= DATA(I) + 180.
         IF (DIF.LT.-270)DATA(I) = DATA(I) - 180.
         IF (DIF.LT.-90) DATA(I) = DATA(I) - 180.
 40      CONTINUE
C                                       And we are ready for the 'final'
C                                       fit.  First, detrend the data.
      DO 45 I = 1,NFREQ
         DATA(I) = DATA(I) + ROTMS * WAVSQ(I)
 45      CONTINUE
      CALL RMSFIT (DATA, SOLTYP, WAVSQ, NFREQ, NPTS, ROTMS, EPA, SIGROT,
     *   SIGPA)
C                                       Calculate the rms error of the
C                                       fit.
 100  PHSUM = 0.
      DO 110 I = NPTS(1),NPTS(2)
         DIF = (DATA(I) - EPA - ROTMS*WAVSQ(I))**2
         PHSUM = PHSUM + DIF
 110     CONTINUE
      PHRMS = SQRT(PHSUM) / (NPTS(2)-NPTS(1)+1)
C
 999  RETURN
      END
      SUBROUTINE RMSFIT (DATA, SOLTYP, WAVSQ, NFREQ, NPTS, ROTMS, EPA,
     *   SIGROT, SIGPA)
C-----------------------------------------------------------------------
C   RMSFIT fits a least squares fit to position angle data as a
C   function of wavelength squared.  It will do an unweighted fit
C   (SOLTYP false) or a fit weighted by the error in the data (SOLTYP
C   true).  In the latter case, sigma maps must be available for each
C   frequency.  We assume the maps are ordered in increasing frequency.
C   Inputs:
C      DATA(20)    R    Input data, the position angles at various
C                       frequencies measured at the same RA and Dec.
C      SOLTYP      L    Solution type: true => do weighted
C      WAVSQ(10)   R    Array of squared wavelengths.
C      NFREQ       I    The number of frequencies.
C      NPTS        I    Use the first NPTS data to do the fit
C   Outputs:
C      ROTMS       R    The rotation measure.
C      EPA         R    Intrinsic value of electric vector.
C      SIGROT      R    Error in the rotation measure
C      SIGPA       R    Error in the position angle
C   The algorithm is lifted straight from Bevington.
C-----------------------------------------------------------------------
      REAL      DATA(*), WAVSQ(*)
      REAL      ROTMS, EPA, SIGROT, SIGPA
      LOGICAL   SOLTYP
      INTEGER   NFREQ, NPTS(2)
C
      REAL      SUM, SUMX, SUMY, SUMX2, SUMY2, SUMXY, VARNCE, A, B, X,
     *   Y, WT, DELTA
      INTEGER   I
C-----------------------------------------------------------------------
C                                       Accumulate the Weighted sums.
      SUM = 0
      SUMX = 0
      SUMY = 0.
      SUMX2 = 0
      SUMXY = 0
      SUMY2 = 0
      DO 10 I = NPTS(1),NPTS(2)
         X = WAVSQ(I)
         Y = DATA(I)
         WT = 1.0
         IF (SOLTYP) WT = 1.0 / (DATA(I+NFREQ)**2)
         SUM = SUM + WT
         SUMX = SUMX + WT*X
         SUMY = SUMY + WT*Y
         SUMX2 = SUMX2 + WT*X*X
         SUMY2 = SUMY2 + WT*Y*Y
         SUMXY = SUMXY + WT*X*Y
 10      CONTINUE
C                                       Coefficients and Errors
      DELTA = SUM*SUMX2 - SUMX*SUMX
      A = (SUMX2*SUMY - SUMX*SUMXY)/DELTA
      B = (SUMXY*SUM - SUMX*SUMY)/DELTA
      VARNCE = 1.
      IF (.NOT.SOLTYP) VARNCE = (SUMY2 + A*A*SUM + B*B*SUMX2
     *   -2.*(A*SUMY + B*SUMXY - A*B*SUMX))/(NPTS(2)-NPTS(1)-1.)
      SIGPA = SQRT(ABS(VARNCE*SUMX2/DELTA))
      SIGROT = SQRT(ABS(VARNCE*SUM/DELTA))
      EPA = A
      ROTMS = B
C                                       Go home
C
 999  RETURN
      END
      SUBROUTINE GETFQV (NFREQ, FREQ, IERR)
C-----------------------------------------------------------------------
C   Gets the frequency values for up to NFREQ entries in the FQ table
C   Inputs:
C      NFREQ    I        Maximum freq id number
C   Outputs
C      FREQ     D(*)     Freq in Hz
C      IERR     I        FQ I/O error code
C-----------------------------------------------------------------------
      INTEGER   NFREQ, IERR
      DOUBLE PRECISION FREQ(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   INSL, INDI, VER, LUN, IFQRNO, FQKOLS(MAXFQC), NROW, I,
     *   FQNUMV(MAXFQC), FQBUFF(512), NUMIF, IFSIDE, FQID
      DOUBLE PRECISION IFFREQ, FREQ0
      REAL      IFCHW, IFTBW
      CHARACTER BNDCOD*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       is there a freq axis
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), I, IERR)
      IF (IERR.NE.0) THEN
         FREQ0 = 0.0D0
      ELSE
         FREQ0 = CATD(KDCRV+I)
         END IF
      INSL = FILTAB(POCAT,6)
      INDI = FILTAB(POVOL,6)
      VER = 1
      LUN = 49
      CALL FQINI ('READ', FQBUFF, INDI, INSL, VER, CATBLK, LUN, IFQRNO,
     *   FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN FQ TABLE'
         GO TO 980
         END IF
      NROW = FQBUFF(5)
      NUMIF = 1
      DO 20 I = 1,NROW
         IFQRNO = I
         CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ FQ TABLE'
            GO TO 980
         ELSE IF ((IERR.EQ.0) .AND. (FQID.GT.0) .AND. (FQID.LE.NFREQ))
     *      THEN
            FREQ(FQID) = IFFREQ + FREQ0
            END IF
 20      CONTINUE
      CALL TABFQ ('CLOS', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF, FQID,
     *   IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSE FQ TABLE'
         CALL MSGWRT (8)
         IERR = 0
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETFQV ERROR',I4,' ON ',A)
      END
      SUBROUTINE ROTFZH (DATA, SOLTYP, WAVSQ, NFREQ, NPTS, RM0, RMMAX,
     *   AROTMS, AEPA, ASIGR, ASIGPA, ACORR, APHRMS, Q0)
C-----------------------------------------------------------------------
C   ROTFZH sets up the data for the least squares fit.  Its most
C   important task is to discover and remove ambiguities from the data.
C   Inputs:
C      DATA     R(*)    Input data, the position angles at various
C                       frequencies measured at the same RA and Dec.
C      SOLTYP   L       Solution type: T => weighted
C      WAVSQ    R(*)    Array of squared wavelengths.
C      RMMAX    R       The maxmium allowed rot. meas.
C      RM0      R       The initial guess of the rot. meas.
C      NFREQ    I       The number of frequencies.
C      NPTS     I(2)    range channels actually used
C   Outputs:
C      AROTMS   R       The rotation measure.
C      AEPA     R       Intrinsic value of electric vector.
C      ASIGR    R       Error in the rotation measure
C      ASIGPA   R       Error in the position angle
C      ACORR    R       Correlation coefficient
C      APHRMS   R       The rms error of the fit
C      Q0       R       Goodness of fit
C-----------------------------------------------------------------------
      LOGICAL   SOLTYP
      INTEGER   NFREQ, NPTS(2)
      REAL      DATA(*), WAVSQ(*), RM0, RMMAX, AROTMS, AEPA, ASIGR,
     *   ASIGPA, ACORR, APHRMS, Q0
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, LPTS(2), NCYC, ICYC, MM, LL
      REAL      ROTMS, EPA, SIGROT, SIGPA, CORR, RDATA(MAXCHA), Q,
     *   PHRMS, DELRM0, CHI2, DIF, RWAVSQ(MAXCHA)
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       For a given biggest rot. meas.
C                                       estimate how many posible turns
C                                       between the first two freq.
      Q0 = 0
      NCYC = 0
      DO 10 I = 1,NFREQ
         RWAVSQ(I) = WAVSQ(I)
 10      CONTINUE
C
      IF (RMMAX.NE.0) NCYC = NINT (RMMAX *
     *   (RWAVSQ(NPTS(1)) - RWAVSQ(NPTS(1)+1)) / 180.0)
      NCYC = ABS (NCYC)
C                                       Try all the possible turns
      DO 200 MM = 1,2*NCYC+1
         DO 20 I = 1,NFREQ
            RDATA(I) = DATA(I)
            RWAVSQ(I) = WAVSQ(I)
            IF (SOLTYP) RDATA(I+NFREQ) = DATA(I+NFREQ)
C                                       Correct the input angles by the
C                                       initial r.m. guess
            RDATA(I) = RDATA(I) - RM0 * RWAVSQ(I)
 20         CONTINUE
C                                       We assume no ambiguities between
C                                       the first two frequencies
         DIF = RDATA(NPTS(1)) - RDATA(NPTS(1)+1)
         IF (DIF.GT.90) RDATA(NPTS(1)+1) = RDATA(NPTS(1)+1) + 180.
         IF (DIF.LT.-90) RDATA(NPTS(1)+1) = RDATA(NPTS(1)+1) - 180.
C                                       Try all the possible turns
         RDATA(NPTS(1)+1) = RDATA(NPTS(1)+1) + (MM-NCYC-1)*180
         DELRM0 = (RDATA(NPTS(1)+1) - RDATA(NPTS(1))) /
     *      (RWAVSQ(NPTS(1)+1) - RWAVSQ(NPTS(1)))
C                                       Now remove additional term
         DO 25 I = 1,NFREQ
            RDATA(I) = RDATA(I) - DELRM0 * RWAVSQ(I)
 25         CONTINUE
C                                       Now remove amgiguities between
C                                       the second and third frequencies
         DIF = RDATA(NPTS(1)+1) - RDATA(NPTS(1)+2)
         ICYC = (ABS(DIF)+90.)/180
         IF (DIF.GT.0) THEN
            RDATA(NPTS(1)+2) = RDATA(NPTS(1)+2)+ICYC*180
         ELSE
            RDATA(NPTS(1)+2) = RDATA(NPTS(1)+2)-ICYC*180
            END IF
C                                       Retrend the DATA to get a
C                                       meaningful corr. coeff.
         DO 30 J = 1,NFREQ
            RDATA(J) = RDATA(J) + (RM0 + DELRM0) * RWAVSQ(J)
 30         CONTINUE
C                                       Do a least squares fit on the
C                                       first 3 points
         DO 50 LL = NPTS(1)+2,NPTS(2)
            LPTS(1) = NPTS(1)
            LPTS(2) = LL
            CALL RMSFZH (RDATA, SOLTYP, RWAVSQ, NFREQ, LPTS, ROTMS, EPA,
     *         SIGROT, SIGPA, CORR, CHI2, Q)
C                                       If we are to fit to 3 points
C                                       only, we are finished.
            IF (NPTS(2).EQ.LL) GO TO 100
C                                       We hope we are close to the
C                                       correct answer. Adjust the data
C                                       by the latest RM estimate
            DO 35 I = 1,NFREQ
               RDATA(I) = RDATA(I) - ROTMS * RWAVSQ(I)
 35            CONTINUE
C                                       Now we correct for those awful
C                                       ambiguities
            DO 40 I = 2,NFREQ
               DIF = RDATA(I-1) - RDATA(I)
               ICYC = (ABS(DIF)+90.)/180
               IF (DIF.GT.0) THEN
                  RDATA(I)=RDATA(I)+ICYC*180
               ELSE
                  RDATA(I) = RDATA(I)-ICYC*180
                  END IF
 40            CONTINUE
C                                       And we are ready for another
C                                       fit.  First, detrend the data.
            DO 45 I = 1,NFREQ
               RDATA(I) = RDATA(I) + ROTMS * RWAVSQ(I)
 45            CONTINUE
 50         CONTINUE
C                                       Calculate the rms error of the
C                                       fit.
 100     PHRMS = SQRT(CHI2) / (NPTS(2)-NPTS(1)+1)
C                                       Using the goodness of fit to
C                                       decide which set of fit to keep
         IF (Q.GE.Q0) THEN
            AROTMS = ROTMS
            AEPA = EPA
            ASIGR = SIGROT
            ASIGPA = SIGPA
            APHRMS = PHRMS
            ACORR = CORR
            Q0 = Q
            END IF
200      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RMSFZH (Y, SOLTYP, X, NFREQ, NPTS, ROTMS, EPA, SIGROT,
     *   SIGPA, CORR, CHI2, Q)
C-----------------------------------------------------------------------
C   RMSFZH fits a linear least squares fit to positions data as a
C   function of wavelength squared. It will do an unweighted fit (SOLTYP
C   false) or a fit weighted by the error in the data (SOLTYP true) and
C   estimate the goodness of fit. In the later case, sigma maps must be
C   available for each frequency.
C   Inputs:
C      Y        R(*)   Input data, the position angles at various
C                      frequencies measured at the same RA and DEC.
C      SOLTYP   L      Solution type: true=> do weighted.
C      X        R(*)   Array of squared wavelengths.
C      NFREQ    I      The number of frequnecies.
C      NPTS     I(2)   Use NPTS(1) - NPTS(2) data to do the fit.
C   Outputs:
C      ROTMS    R      The rotation measure.
C      EPA      R      Intrinsic value of electric vector.
C      SIGROT   R      Error in the rotation measure.
C      SIGPA    R      Error in the position angle.
C      CORR     R      Correlation coefficient.
C   The program is modified from "Numerical Recipies"
C------------------------------------------------------------------------
      REAL      Y(*), X(*), ROTMS, EPA, SIGROT, SIGPA, CORR, CHI2, Q
      LOGICAL   SOLTYP
      INTEGER   NFREQ, NPTS(2)
C
      INTEGER   I
      REAL      WT, COV, SX, SY, ST2, B, SS, SXOSS, T, A, SIGDAT, GAMMQ
C-----------------------------------------------------------------------
      SX = 0.
      SY = 0.
      ST2 = 0.
      B = 0.
      SS = 0.
      WT = 1.0
      DO 10 I = NPTS(1),NPTS(2)
         IF (SOLTYP) WT = 1. / (Y(I+NFREQ)**2)
         SS = SS + WT
         SX = SX + X(I) * WT
         SY = SY + Y(I) * WT
 10      CONTINUE
      SXOSS = SX/SS
      DO 15 I = NPTS(1),NPTS(2)
         IF (SOLTYP) WT = 1. / Y(I+NFREQ)
         T = (X(I) - SXOSS) * WT
         ST2 = ST2 + T * T
         B = B + T * Y(I) * WT
 15      CONTINUE
      B = B / ST2
      A = (SY - SX * B) / SS
      EPA = A
      ROTMS = B
      SIGPA = SQRT ((1.+ SX*SX / (SS*ST2)) / SS)
      SIGROT = SQRT (1./ST2)
      CHI2 = 0.
      IF (.NOT.SOLTYP) THEN
         DO 20 I = NPTS(1),NPTS(2)
            CHI2 = CHI2 + (Y(I) - A - B*X(I))**2
 20         CONTINUE
         Q = 1.
         SIGDAT = SQRT (CHI2 / (NPTS(2)-NPTS(1)-1.))
         SIGPA = SIGPA * SIGDAT
         SIGROT = SIGROT * SIGDAT
      ELSE
         DO 25 I = NPTS(1),NPTS(2)
            CHI2 = CHI2 + ((Y(I) - A - B*X(I)) / Y(I+NFREQ))**2
 25         CONTINUE
         Q = GAMMQ (0.5*(NPTS(2)-NPTS(1)-1), 0.5*CHI2)
         END IF
      COV = -SX / (SS * ST2)
      CORR = COV / (SIGPA * SIGROT)
C
 999  RETURN
      END
      REAL FUNCTION GAMMQ (A, X)
C-----------------------------------------------------------------------
C   Functions to calculate the goodness of fit using chi-square
c   merit. The program is lifted from "Numerical recipies"
c   Press, Flannery, Teukolsky, Vetterling; Chapter 6.2; Chapter 14.2,
c   Page 507, (1986).
C------------------------------------------------------------------------
      REAL      A, X
C
      REAL      GAMSER, GLN
C-----------------------------------------------------------------------
      IF ((X.GE.0.) .AND. (A.GT.0.)) THEN
         IF (X.LT.A+1.) THEN
            CALL GSER (GAMSER, A, X, GLN)
            GAMMQ = 1. - GAMSER
         ELSE
            CALL GCF (GAMMQ, A, X, GLN)
            END IF
      ELSE
         GAMMQ = -1.0
         END IF
C
 999  RETURN
      END
      REAL FUNCTION GAMMLN (XX)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      REAL      XX
C
      DOUBLE PRECISION COF(6), STP, HALF, ONE, FPF, X, TMP, SER
      INTEGER   J
      DATA COF, STP/76.18009173D0, -86.50532033D0, 24.01409822D0,
     *    -1.231739516D0, .120858003D-2, -.536382D-5, 2.50662827465D0/
      DATA HALF, ONE, FPF /0.5D0, 1.0D0, 5.5D0/
C-----------------------------------------------------------------------
      X = XX - ONE
      TMP = X + FPF
      TMP = (X+HALF) * LOG (TMP) - TMP
      SER = ONE
      DO 10 J = 1,6
         X = X + ONE
         SER = SER + COF(J) / X
 10      CONTINUE
      GAMMLN = TMP + LOG (STP*SER)
C
 999  RETURN
      END
      SUBROUTINE GSER (GAMSER, A, X, GLN)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      REAL      GAMSER, A, X, GLN
C
      INTEGER   ITMAX, N
      REAL      EPS, GAMMLN, AP, SUM, DEL
      INCLUDE 'INCS:DMSG.INC'
      PARAMETER (ITMAX = 100, EPS = 3.E-7)
C-----------------------------------------------------------------------
      GLN = GAMMLN (A)
      IF (X.LE.0.) THEN
         GAMSER = 0.
      ELSE
         AP = A
         SUM = 1./A
         DEL = SUM
         DO 10 N = 1,ITMAX
            AP = AP + 1.
            DEL = DEL * X / AP
            SUM = SUM + DEL
            IF (ABS(DEL).LT.ABS(SUM)*EPS) GO TO 20
 10         CONTINUE
         MSGTXT = 'GSER: A too large, ITMAX too small'
         CALL MSGWRT (8)
 20      GAMSER = SUM * EXP (-X + A *LOG(X) - GLN)
         END IF
C
 999  RETURN
      END
      SUBROUTINE GCF (GAMMCF, A, X, GLN)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      REAL      GAMMCF, A, X, GLN
C
      INTEGER   ITMAX, N
      REAL      EPS, GAMMLN, GOLD, A0, A1, B0, B1, FAC, AN, ANA, ANF, G
      PARAMETER (ITMAX = 100, EPS = 3.E-7)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      GLN = GAMMLN (A)
      GOLD = 0.
      A0 = 1.
      A1 = X
      B0 = 0.
      B1 = 1.
      FAC = 1.
      DO 10 N = 1,ITMAX
         AN = FLOAT (N)
         ANA = AN - A
         A0 = (A1 + A0*ANA) * FAC
         B0 = (B1 + B0*ANA) * FAC
         ANF = AN * FAC
         A1 = X * A0 + ANF * A1
         B1 = X * B0 + ANF * B1
         IF (A1.NE.0.) THEN
            FAC = 1. / A1
            G = B1 * FAC
            IF (ABS((G-GOLD)/G).LT.EPS) GO TO 20
            GOLD = G
            END IF
 10      CONTINUE
      MSGTXT = 'GCF: A too large, ITMAX too small'
      CALL MSGWRT (8)
 20   GAMMCF = EXP (-X + A * ALOG(X) - GLN) * G
C
 999  RETURN
      END

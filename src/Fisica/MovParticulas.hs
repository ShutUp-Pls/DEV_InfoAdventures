module Fisica.MovParticulas where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro         as LMi
import qualified Linear.Vector      as LV
import qualified Control.Monad.State as CMS
-- Módulos propios
import qualified Objetos.Types      as OType
import qualified Globals.Types      as GType

actualizarParticulas :: Float -> [OType.Particula] -> [OType.Particula]
actualizarParticulas dt particulas = filter particulaViva $ map (pureMoverParticula dt) particulas

particulaViva :: OType.Particula -> Bool
particulaViva p = (p LMi.^. OType.parEnt . GType.entVid . GType.vidAct) > 0

pureMoverParticula :: Float -> OType.Particula -> OType.Particula
pureMoverParticula dt p = CMS.execState (moverParticula dt) p

moverParticula :: Float -> CMS.State OType.Particula ()
moverParticula dt = do
    tip         <- CMS.gets (LMi.^. OType.parTip)
    pos         <- CMS.gets (LMi.^. OType.parEnt . GType.entBox . GType.boxPos)
    angleDeg    <- CMS.gets (LMi.^. OType.parEnt . GType.entBox . GType.boxAng)
    speed       <- CMS.gets (LMi.^. OType.parEnt . GType.entMov . GType.movVel)
    vidaAct     <- CMS.gets (LMi.^. OType.parEnt . GType.entVid . GType.vidAct)

    let angleRad    = angleDeg * (pi / 180.0)
        dir         = SDL.V2 (cos angleRad) (sin angleRad)
        vidaNew     = vidaAct - dt
        posNew      = pos + dir LV.^* (speed * dt)

    case tip of
        OType.MovimientoLineal ->
            CMS.modify $ \p ->
                p LMi.& OType.parEnt . GType.entBox . GType.boxPos LMi..~ posNew
                  LMi.& OType.parEnt . GType.entVid . GType.vidAct LMi..~ vidaNew

        OType.MovimientoGradualDown ->
            CMS.modify $ \p ->
                p LMi.& OType.parEnt . GType.entBox . GType.boxPos LMi..~ posNew
                  LMi.& OType.parEnt . GType.entVid . GType.vidAct LMi..~ vidaNew
                  LMi.& OType.parEnt . GType.entMov . GType.movVel LMi.%~ (* 0.90)
                  LMi.& OType.parEnt . GType.entBox . GType.boxTam  LMi.%~ (LV.^* 0.95)


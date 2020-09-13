#ifndef SCENEMODIFIER_H
#define SCENEMODIFIER_H

#include <QObject>

#include <Qt3DCore>
#include <Qt3DExtras>
#include <Qt3DRender>

class SceneModifier : public QObject
{
    Q_OBJECT
    Qt3DCore::QEntity *m_rootEntity;
    SceneModifier *modifier;

    Qt3DExtras::QTorusMesh *m_torus;
    Qt3DCore::QEntity *m_torusEntity;
    Qt3DCore::QTransform *torusTransform;
    Qt3DExtras::QPhongMaterial *torusMaterial;

public:
    explicit SceneModifier(Qt3DCore::QEntity *rootEntity);
    ~SceneModifier();
};

#endif // SCENEMODIFIER_H

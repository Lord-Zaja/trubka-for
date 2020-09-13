#include "scenemodifier.h"
#include <QtDebug>
SceneModifier::SceneModifier(Qt3DCore::QEntity *rootEntity) : m_rootEntity(rootEntity)
{
    /*
    // Torus shape data
        m_torus = new Qt3DExtras::QTorusMesh;
        m_torus->setRadius(1.0f);
        m_torus->setMinorRadius(0.4f);
        m_torus->setRings(100);
        m_torus->setSlices(20);

        // TorusMesh Transform
        torusTransform = new Qt3DCore::QTransform;
        torusTransform->setScale(2.0f);
        torusTransform->setRotation(QQuaternion::fromAxisAndAngle(QVector3D(0.0f, 1.0f, 0.0f), 25.0f));
        torusTransform->setTranslation(QVector3D(5.0f, 4.0f, 0.0f));

        torusMaterial = new Qt3DExtras::QPhongMaterial;
        torusMaterial->setDiffuse(QColor(QRgb(0xbeb32b)));

        // Torus
        m_torusEntity = new Qt3DCore::QEntity(m_rootEntity);
        m_torusEntity->addComponent(m_torus);
        m_torusEntity->addComponent(torusMaterial);
        m_torusEntity->addComponent(torusTransform);
        m_torusEntity->setEnabled(true);*/
    // Torus shape data
    //! [0]
    m_torus = new Qt3DExtras::QTorusMesh();
    m_torus->setRadius(1.0f);
    m_torus->setMinorRadius(0.4f);
    m_torus->setRings(100);
    m_torus->setSlices(20);
    //! [0]

    // TorusMesh Transform
    //! [1]
    Qt3DCore::QTransform *torusTransform = new Qt3DCore::QTransform();
    torusTransform->setScale(2.0f);
    torusTransform->setRotation(QQuaternion::fromAxisAndAngle(QVector3D(0.0f, 1.0f, 0.0f), 25.0f));
    torusTransform->setTranslation(QVector3D(5.0f, 4.0f, 0.0f));
    //! [1]

    //! [2]
    Qt3DExtras::QPhongMaterial *torusMaterial = new Qt3DExtras::QPhongMaterial();
    torusMaterial->setDiffuse(QColor(QRgb(0xbeb32b)));
    //! [2]

    // Torus
    //! [3]
    m_torusEntity = new Qt3DCore::QEntity(m_rootEntity);
    m_torusEntity->addComponent(m_torus);
    m_torusEntity->addComponent(torusMaterial);
    m_torusEntity->addComponent(torusTransform);


}

SceneModifier::~SceneModifier()
{
}

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

#include<QMenu>
#include<QMenuBar>

#include <Qt3DExtras>
#include <Qt3DCore>
#include <Qt3DRender>

//class QQuickWidget;
class SceneModifier;

class MainWindow : public QMainWindow
{
    Q_OBJECT
    void setupForm();
    void createActions();

    QMenu *mnuFile;

    Qt3DExtras::Qt3DWindow *view;
    Qt3DCore::QEntity *rootEntity;
    Qt3DRender::QCamera *cameraEntity;
    Qt3DExtras::QOrbitCameraController *camController;
    SceneModifier *modifier;
    Qt3DCore::QEntity *lightEntity;
    Qt3DRender::QPointLight *light;
    Qt3DCore::QTransform *lightTransform;
public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
};
#endif // MAINWINDOW_H

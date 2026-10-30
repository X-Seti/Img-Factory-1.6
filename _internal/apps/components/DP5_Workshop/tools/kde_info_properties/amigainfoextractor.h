// this belongs in apps/components/DP5_Workshop/tools/kde_info_properties/amigainfoextractor.h - Version: 1
// X-Seti - October 27 2026 - IMG Factory 1.6 - Amiga .info properties for Dolphin / KDE.

#pragma once

#include <KFileMetaData/ExtractorPlugin>

#ifndef kfilemetadata_extractor_iid
#define kfilemetadata_extractor_iid "org.kde.kf5.kfilemetadata.ExtractorPlugin"
#endif

// Methods list -
// AmigaInfoExtractor::AmigaInfoExtractor
// AmigaInfoExtractor::extract
// AmigaInfoExtractor::mimetypes

class AmigaInfoExtractor : public KFileMetaData::ExtractorPlugin
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID kfilemetadata_extractor_iid FILE "amigainfoextractor.json")
    Q_INTERFACES(KFileMetaData::ExtractorPlugin)

public:
    explicit AmigaInfoExtractor(QObject *parent = nullptr);
    QStringList mimetypes() const override;
    void extract(KFileMetaData::ExtractionResult *result) override;
};

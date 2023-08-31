package com.cf.parking.api.response;

import java.io.Serializable;

public class UploadUrlRsp implements Serializable {
    private String path;

    private String ossUrl;

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getOssUrl() {
        return ossUrl;
    }

    public void setOssUrl(String ossUrl) {
        this.ossUrl = ossUrl;
    }
}
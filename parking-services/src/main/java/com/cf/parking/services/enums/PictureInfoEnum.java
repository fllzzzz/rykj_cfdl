package com.cf.parking.services.enums;

/**
 * 图片信息
 * @author
 * @date 2023/9/18
 */
public enum PictureInfoEnum {
    CONTENT_TYPE_JPG("image/jpeg","jpg图片类型"),
    CONTENT_TYPE_PNG("image/png","png图片类型"),

    BASE64_JPG_PRE("data:image/jpeg;base64,","jpg图片转base64拼接前缀"),
    BASE64_PNG_PRE("data:image/png;base64,","png图片转base64拼接前缀"),;


    private String info;

    private String remark;

    private PictureInfoEnum(String info, String remark) {
        this.info = info;
        this.remark = remark;
    }

    public String getInfo() {
        return info;
    }

    public void setInfo(String info) {
        this.info = info;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }

}

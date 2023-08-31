package com.cf.parking.facade.enums;

public enum UploadFileTypeEnum {
    CERT_PICTURE(1, "上传驾驶证、行驶证照片","certImg/"),
    HEAD_PORTRAIT(2, "上传头像路径","avatarUrl/"),
    TMP_FILE(9, "临时文件路径(到时间自动过期)","tmpfile/"),
    ;

    private final Integer value;
    private final String name;
    private final String path;

    UploadFileTypeEnum(Integer value, String name, String path) {
        this.value = value;
        this.name = name;
        this.path = path;
    }

    public Integer value(){
        return value;
    }

    public String path(){
        return path;
    }


    public static String getPath(Integer value) {
        for (UploadFileTypeEnum ele : values()) {
            if (ele.value().equals(value)){
                return ele.path();
            }
        }
        return null;
    }
}

package com.cf.parking.facade.bo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * @author
 * @date 2023/9/26
 */
@Data
public class ParkingLotImageBO {
    /** id */
    private Long id;

    /** parentId */
    private Long parentId;

    /** parentName */
    private String parentName;

    /** 区域 */
    private String region;

    /** 区域编号 */
    private String regionCode;

    /** 车位数量 */
    private Long amount;

    /** 类型(0：可摇号，1：不可摇号) */
    private String type;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    /** 备注 */
    private String remark;

    /** 停车场图片信息 */
    private List<ParkingLotImageInfoBO> files;
}

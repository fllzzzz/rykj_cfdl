package com.cf.parking.api.request;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.Date;
import java.util.List;

/**
 * 车辆审核操作对象
 * @author
 * @date 2023/9/7
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "车辆审核操作对象（新增/审核）")
public class UserVerifyOptReq {

    /** 批量审核的ids */
    @ApiModelProperty(value = "批量审核时传入的ids")
    private List<Long> ids;

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /**
     * userID
     *
     */
    @ApiModelProperty(value = "userID")
    private Long userId;

    /**
     * 申请人
     */
    @ApiModelProperty(value = "申请人")
    private String userName;

    /**
     * 车牌号
     */
    @ApiModelProperty(value = "车牌号")
    private String plateNo;

    /**
     * 车辆照片
     */
    @ApiModelProperty(value = "车辆照片")
    private String vehicleImg;

    /**
     * 行驶证照片path
     */
    @ApiModelProperty(value = "行驶证照片")
    private String drivingPermitImg;

    /**
     * 驾驶证照片path
     */
    @ApiModelProperty(value = "驾驶证照片")
    private String drivingLicenseImg;

    /**
     * 状态(0:默认，1:待审核，2:审核不通过,3:审核通过)
     */
    @ApiModelProperty(value = "状态(0:默认，1:待审核，2:审核不通过,3:审核通过)")
    private Integer state;

    /**
     * 审核意见
     */
    @ApiModelProperty(value = "审核意见")
    private String reason;

    /**
     * 申请日期
     */
    @ApiModelProperty(value = "申请日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}

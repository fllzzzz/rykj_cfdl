package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.util.Date;

/**
 * 车辆审核导出结果
 * @author
 * @date 2023/9/25
 */
@Data
public class ExportUserVerifyRsp {

    /** 申请日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    @Excel(name = "申请日期", orderNum = "1")
    private Date createTm;

    /**
     * 申请人
     */
    @Excel(name = "申请人", orderNum = "2")
    private String userName;

    /**
     * 车牌号
     */
    @Excel(name = "车牌号", orderNum = "3")
    private String plateNo;

    /**
     * 车辆照片
     */
    @Excel(name = "车辆照片", orderNum = "4")
    private String vehicleImg;

    /**
     * 行驶证照片path
     */
    @Excel(name = "行驶证照片", orderNum = "5")
    private String drivingPermitImg;

    /**
     * 驾驶证照片path
     */
    @Excel(name = "驾驶证照片", orderNum = "6")
    private String drivingLicenseImg;

    /**
     * 状态(0:默认，1:待审核，2:审核不通过,3:审核通过)
     */
    @Excel(name = "状态", orderNum = "7")
    private Integer state;

    /**
     * 审核意见
     */
    @Excel(name = "审核意见", orderNum = "8")
    private String reason;


}
